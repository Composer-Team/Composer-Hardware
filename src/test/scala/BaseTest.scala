import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util._
import composer.MemoryStreams._
import scala.util.Random
import composer._

case class LFSRConfig(length: Int, taps: Seq[Int])

// galois lfsr
// note to self: look at dramsim2
class LFSRCore(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  // simple state machine - not composer specific, but useful anyways
  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // you can use configs to access implementation details for your core (optional)
  val conf = p(LFSRConfigKey)
  val taps = conf.taps map (_ - 1)
  val rand_gen = new Random()

  // bog-standard LFSR
  val lfsr = Seq.fill(conf.length)(RegInit(rand_gen.nextBoolean().B))
  val outputbit = lfsr(0)
  val outputCache = Seq.fill(conf.length)(RegInit(false.B))
  val count = RegInit(0.U(log2Up(conf.length).W))

  // REQUIRED: you MUST tie-off the cmd/response interface
  // here we're using the Chisel last-connect semantics to define the default values for each of the wires
  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.data := 0.U
  io.resp.bits.rd := 0.U
  io.busy := true.B

  // when we're idle we accept commands
  when(state === s_idle) {
    io.busy := false.B
    io.req.ready := true.B
    when(io.req.fire) {
      state := s_working
      count := (conf.length - 1).U
    }
  }.elsewhen(state === s_working) {
    // when we're working we tick the LFSR
    (0 until conf.length - 1).foreach { i =>
      lfsr(i) := lfsr(i + 1)
      outputCache(i) := outputCache(i + 1)
    }
    outputCache(conf.length - 1) := outputbit
    taps foreach { tap =>
      if (tap == (conf.length - 1)) {
        lfsr(tap) := outputbit
      } else {
        lfsr(tap) := lfsr(tap + 1) ^ outputbit
      }
    }
    count := count - 1.U
    when(count === 0.U) {
      state := s_finish
    }
  }.elsewhen(state === s_finish) {
    // when we're done we signal response what we want to send back to the CPU
    io.resp.bits.data := VecInit(outputCache).asUInt
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}

// * * * * * * * * * * * * * * * * * * * * * * * * * * Simple ALU Implementation * * * * * * * * * * * * * * * * * * *

class SimpleALU(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  // again, we define some sort of state machine
  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // a request will carry our operation, two operands, and we will return a result
  val op = RegInit(0.U(io.req.bits.inst.rs1.getWidth.W))
  val a = RegInit(0.U(32.W))
  val b = RegInit(0.U(32.W))
  val result = RegInit(0.U(32.W))

  // tie-off the required cmd/response signals. We assign them to false by default because we're going to use
  // Chisel last-connect semantics to set them to high under certain conditions (for instance: if we're in the finishing
  // state then io.resp.valid will be true
  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.data := 0.U
  io.resp.bits.rd := 0.U
  io.busy := true.B

  when(state === s_idle) {
    io.busy := false.B
    io.req.ready := true.B
    // .fire means (.ready && .valid)
    when(io.req.fire) {
      // store our request in registers - if we don't handshake by signaling ready then the machine will stall!
      state := s_working
      op := io.req.bits.inst.rs1
      a := io.req.bits.payload1
      b := io.req.bits.payload2
    }
  }.elsewhen(state === s_working) {
    // perform our operation
    switch(op) {
      is(0.U) {
        result := a +& b
      }
      is(1.U) {
        result := a -& b
      }
      is(2.U) {
        result := a * b
      }
    }
    state := s_finish
  }.elsewhen(state === s_finish) {
    // resp is valid and we set it to our result value
    io.resp.bits.data := result(31, 0)
    io.resp.valid := true.B
    when(io.resp.fire) {
      // make sure you return to your idle state and reset anything you need to
      state := s_idle
    }
  }
}

/*
 * * * * * * * * * * * * * * * * * * * * * VECTOR ADDER * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

case class VectorConfig(dWidth: Int, // what is the datatype width?
                       )

class VectorAdder(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  // state machine is a little more complicated this time
  val s_idle :: s_load :: s_add :: s_store :: s_commit :: s_finish :: Nil = Enum(6)
  val vConfig = p(VectorAdderKey)
  require(isPow2(vConfig.dWidth), "The vector data width must be a power of two!")
  require(vConfig.dWidth % 8 == 0)
  val vDivs = 8

  // We get a group of reader/writer channels using this interface. The size of the group is defined in the config
  // dataBytes is the width of a datum
  // vlen is the number of datums to read on a single handshake
  // this functionality is mostly useful so that you can read multiple inputs in parallel without having to do so much
  // manual packing/unpacking
  val (_, myReaders) = getReaderModules(
    name = "ReadChannel",
    useSoftwareAddressing = true,
    dataBytes = vConfig.dWidth / 8,
    vlen = vDivs)
  val (_, myWriters) = getWriterModules(
    name ="WriteChannel",
    useSoftwareAddressing = true,
    dataBytes = vConfig.dWidth / 8 * vDivs)
  // For this module the groups are just size 1
  // These modules are going to use software addressing which means that we supply the addresses and lengths for the
  // reads via the interface in software (see composer::rocc::addr_cmd).
  // addr_cmd takes two parameters: a `ComposerAddrInfo` struct, and the pointer (with implicit length information) that
  // you will supply to the reader
  //
  // How do you make a `ComposerAddrInfo` struct? With the `getChannelSubIdx` function (generated in the
  // #include <composer_allocator_declaration.h> header. This function takes the following arguments.
  //
  // - system ID - which system are we (in this case we'll supply VectorSystem_ID to this parameter because that's us)
  // - core ID - which core are we?
  // - channel name - the name of the group of channels are generated into an Enum that you can reference via the header
  //                    #include <composer_allocator_declaration.h> in the enum ComposerChannels.
  //                    so for this case, we would supply either ComposerChannels::ReadChannel or ComposerChannels::WriteChannel
  // - id - index of the channel within the group
  //
  // Generally, I would actually recommend using hardware addressing (passing in your addresses and lengths manually
  // through start commands becuase I think it's easier to debug, but this is a legacy functionality from Composer that
  // has made it into its current implementation

  val myReader = myReaders(0)
  val myWriter = myWriters(0)
  val state = RegInit(s_idle)
  val toAdd = Reg(UInt(vConfig.dWidth.W))
  val vLen = Reg(UInt(io.req.bits.payload1.getWidth.W))
  val rfinish = RegInit(false.B)

  // like previous examples, make sure you tie off your req/resp interfaces as well as `busy`
  io.req.ready := state === s_idle
  io.busy := state =/= s_idle
  io.resp.valid := false.B // set again later under s_finish
  io.resp.bits.data := 0.U
  io.resp.bits.rd := 0.U

  val dArray = Seq.fill(vDivs)(Reg(UInt(vConfig.dWidth.W)))

  // user reverse to maintain order
  myWriter.data.bits := Cat(dArray.reverse)
  // the writer module will cache lines until a line has been completely written
  // set finishEarly when you want to flush the current transaction
  myWriter.finishEarly := state === s_idle
  myWriter.data.valid := false.B
  // stop a reader if you want to finish the transaction prematurely
  myReader.stop := false.B
  myReader.data.ready := false.B

  when(state === s_idle) {
    // don't start if it's a 0-lengthed segment
    when(io.req.fire && io.req.bits.payload1 =/= 0.U) {
      // start address has already been loaded into reader, just need to start reading
      state := s_load
      toAdd := io.req.bits.payload2(vConfig.dWidth - 1, 0)
      vLen := io.req.bits.payload1
      rfinish := false.B
    }
  }.elsewhen(state === s_load) {
    myReader.data.ready := true.B
    when(myReader.data.fire) {
      // break the input into data-sized segments defined by vConfig.dwidth
      dArray zip myReader.data.bits foreach { case (reg: UInt, dat: UInt) =>
        reg := dat
      }
      state := s_add
      vLen := vLen - vDivs.U
      when(vLen === vDivs.U) {
        rfinish := true.B
      }
    }
  }.elsewhen(state === s_add) {
    dArray.foreach { a: UInt => a := a + toAdd }
    state := s_store
  }.elsewhen(state === s_store) {
    myWriter.data.valid := true.B
    when(myWriter.data.fire) {
      when(rfinish) {
        state := s_commit
      }.otherwise {
        state := s_load
      }
    }
  }.elsewhen(state === s_commit) {
    when(myWriter.channelIdle) {
      state := s_finish
    }
  }.elsewhen(state === s_finish) {
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}


case object LFSRConfigKey extends Field[LFSRConfig]

case object VectorAdderKey extends Field[VectorConfig]

class WithLFSR(withNCores: Int) extends Config((site, _, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(),
    nCores = withNCores,
    name = "LFSRSystem",
    buildCore = {
      case (coreParams: ComposerConstructor, parameters: Parameters) =>
        new LFSRCore(coreParams)(parameters)
    }))

  case LFSRConfigKey => LFSRConfig(7, Seq(7, 6, 5, 4))
})

class WithALUs(withNCores: Int) extends Config((site, _, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(),
    nCores = withNCores,
    name = "ALUSystem",
    buildCore = {
      case (coreParams: ComposerConstructor, parameters: Parameters) =>
        new SimpleALU(coreParams)(parameters)
    }))
})

class WithVectorAdder(withNCores: Int, dataWidth: Int) extends Config((site, _, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      memoryChannelParams = List(
        CReadChannelParams("ReadChannel", 1),
        CWriteChannelParams("WriteChannel", 1))
    ),
    nCores = withNCores,
    name = "VectorSystem",
    buildCore = {
      case (composerCoreParams: ComposerConstructor, parameters: Parameters) =>
        new VectorAdder(composerCoreParams)(parameters)
    }
  ))

  // use 8-bit data divisions
  case VectorAdderKey => VectorConfig(dWidth = dataWidth)
})

class exampleConfig extends Config(
  // example configuration that has
  //  - 1 SimpleALU that supports add, sub, multiply
  //  - 1 VectorAdder that uses Readers/Writers to read/write to large chunks of memory
  //  - 1 GaloisLFSR that returns random numbers over the command/response interface
  new WithVectorAdder(1, 16) ++ new WithALUs(1) ++ new WithLFSR(1) ++ new WithComposer() ++ new WithAWSPlatform(1)
)

class exampleConfigKria extends Config(
  // example configuration that has
  //  - 1 SimpleALU that supports add, sub, multiply
  //  - 1 VectorAdder that uses Readers/Writers to read/write to large chunks of memory
  //  - 1 GaloisLFSR that returns random numbers over the command/response interface
  new WithVectorAdder(1, 16) ++ new WithALUs(1) ++ new WithComposer() ++ new WithKriaPlatform()
)

// Drivers
object td extends App {
  TestDriver.buildConfig(new exampleConfigKria)
}
