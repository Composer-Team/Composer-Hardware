package design.machsuite.Gemm

import chipsalliance.rocketchip.config._
import composer._
import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import composer.RoccHelpers.{ComposerFunc, ComposerOpcode}
import freechips.rocketchip.subsystem.ExtMem

class GemmTarget(val rcBits: Int) extends Bundle {
  val row = UInt(rcBits.W)
  val col = UInt(rcBits.W)
}


/*
let's say the width is max 8 bits (enough to store inclusive of the limit)
e.x. 16 is width 5 (accounting for inclusivity)
and our matrix min width is 4
then the maxDivs is 4
Whenever we get the number 16 in from witdth, we need to figure out from that how many divs we're considering for the
run
16: 1000
Now, getting 4 as the answer would require us taking one more bit than necessary. If we just -1, then we get the right
answer with 1 less bit
So then our logical div ceiling is matSize(width-1, log2Up(minMatSize))-1

 */


class TargetChooser(maxDivs: Int) extends Module {
  require(isPow2(maxDivs))
  val rcBits = log2Up(maxDivs)

  val init = IO(Flipped(Valid(UInt(rcBits.W))))
  val out = IO(Decoupled(new GemmTarget(rcBits)))
  val completeSignal = IO(Output(Bool()))

  val complete = Reg(Bool())
  completeSignal := complete

  val countRow = Reg(UInt(rcBits.W))
  val countCol = Reg(UInt(rcBits.W))
  val nextColStart = Reg(UInt(rcBits.W))
  val dimMax = Reg(UInt(rcBits.W))

  when(init.valid) {
    dimMax := init.bits
    countCol := 0.U
    countRow := 0.U
    nextColStart := 1.U
    complete := false.B
  }

  out.bits.col := countCol
  out.bits.row := countRow
  out.valid := !complete

  when(out.fire) {
    when(countRow === dimMax) {
      when(nextColStart === 0.U) {
        complete := true.B
      }.otherwise {
        countCol := nextColStart
        when (nextColStart === dimMax) {
          nextColStart := 0.U
        }.otherwise {
          nextColStart := nextColStart + 1.U
        }
        countRow := 0.U
      }
    }.otherwise {
      countRow := countRow + 1.U
      when(countCol === dimMax) {
        countCol := 0.U
      }.otherwise {
        countCol := countCol + 1.U
      }
    }
  }
}

object GemmDispatcherCommands extends ChiselEnum {
  val AB, CSizeAndGo = Value
}

class GemmDispatcher(composerConstructor: ComposerConstructor, gp: GemmParam)(implicit p: Parameters)
  extends ComposerCore(composerConstructor) {
  def isPow2UInt(a: UInt): Bool = {
    (a & (a - 1.U)) === 0.U
  }

  val mySystem = p(ComposerSystemsKey).filter(_.name == "GemmCore")(0)
  val nCores = mySystem.nCores
  val addressBits = log2Up(p(ExtMem).get.master.size)

  val coreIdleBits = RegInit(VecInit(Seq.fill(nCores)(true.B)))
  val haveCoreAvailable = coreIdleBits.reduce(_ || _)

  val s_idle :: s_init :: s_allocate :: s_address :: s_finishing :: Nil = Enum(5)
  val state = RegInit(s_idle)
  CppGeneration.exportChiselEnum(GemmDispatcherCommands)

  val ABase = Reg(UInt(addressBits.W))
  val BBase = Reg(UInt(addressBits.W))
  val CBase = Reg(UInt(addressBits.W))
  val sizeWidth = log2Up(gp.maxRCDim) + 1
  CppGeneration.addUserCppDefinition("int", "GemmMatSizeWidth", sizeWidth)
  val matSize = Reg(UInt(sizeWidth.W))

  val divs = gp.maxRCDim / gp.rowColDim
  val targetter = Module(new TargetChooser(divs))
  val target = Reg(new GemmTarget(rcBits = targetter.rcBits))
  val chosenCore = Reg(UInt(log2Up(nCores).W))
  val progressStoppage = matSize(sizeWidth - 1, log2Up(gp.rowColDim)) - 1.U

  targetter.init.bits := progressStoppage
  targetter.init.valid := false.B
  targetter.out.ready := false.B

  val coreBuffers = Reg(Vec(nCores, new Bundle {
    val progress = UInt(targetter.rcBits.W)
    val A, B, C = UInt(addressBits.W)
  }))

  val dispatchWire = Wire(Decoupled(UInt(log2Up(nCores).W)))
  dispatchWire.bits := DontCare
  dispatchWire.valid := false.B
  val dispatchQueue = Queue(dispatchWire, entries = nCores)
  dispatchQueue.ready := false.B

  io.req.ready := state === s_idle
  io.resp.bits := DontCare
  io.resp.valid := false.B

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        val inst = io.req.bits.inst
        val payload = Cat(io.req.bits.payload1, io.req.bits.payload2)
        switch(GemmDispatcherCommands(inst.rs1(GemmDispatcherCommands.getWidth-1, 0))) {
          is(GemmDispatcherCommands.AB) {
            BBase := payload(addressBits - 1, 0)
            ABase := payload(addressBits * 2 - 1, addressBits)
          }
          is(GemmDispatcherCommands.CSizeAndGo) {
            CBase := payload(addressBits + sizeWidth - 1, sizeWidth)
            val matS = payload(sizeWidth - 1, 0)
            matSize := matS
            assert(matS > gp.rowColDim.U && isPow2UInt(matS))
            state := s_init
          }
        }
      }
    }
    is(s_init) {
      targetter.init.valid := true.B
      state := s_allocate
    }
    is(s_allocate) {
      targetter.out.ready := haveCoreAvailable
      when(targetter.out.fire) {
        target := targetter.out.bits
        state := s_address
        val core = PriorityEncoder(coreIdleBits)
        chosenCore := core
        coreIdleBits(core) := false.B
        coreBuffers(core).progress := 0.U
      }
      when(targetter.completeSignal) {
        state := s_finishing
      }
    }
    is(s_address) {
      // target_row * dim gives the row number. Multiply by the size of the matrix row to give the offset
      // add two more zeroes (*4) to account for datatype width
      val row_num = Cat(target.row, 0.U(log2Up(gp.rowColDim).W))
      val row_off = Cat(row_num * matSize, 0.U(2.W))
      coreBuffers(chosenCore).A := ABase + row_off

      val col_off = Cat(target.col, 0.U((2 + log2Up(gp.rowColDim)).W))
      coreBuffers(chosenCore).B := BBase + col_off

      coreBuffers(chosenCore).C := CBase + row_off + col_off

      when(!composer_response_io.valid) {
        state := s_allocate
        dispatchWire.bits := chosenCore
        dispatchWire.valid := true.B
      }
      assert(dispatchWire.ready)
    }
    is(s_finishing) {
      when(coreIdleBits.reduce(_ && _)) {
        io.resp.valid := true.B
        io.resp.bits := DontCare
        when(io.resp.fire) {
          state := s_idle
        }
      }
    }
  }

  // false means inactive
  val dispatch_idle :: dispatch_A :: dispatch_B :: dispatch_C :: dispatch_sizeAndGo :: Nil = Enum(5)
  val dState = RegInit(dispatch_idle)
  val to_dispatch = dispatchQueue.bits
  val cmd = composer_command_io
  cmd.valid := false.B
  cmd.bits.inst.system_id := getSystemID("GemmCore")
  cmd.bits.inst.funct := ComposerFunc.START.U
  cmd.bits.inst.opcode := ComposerOpcode.ACCEL
  cmd.bits.inst.rs1 := DontCare // this will be overset
  cmd.bits.inst.rs2 := DontCare
  cmd.bits.inst.xd := false.B
  cmd.bits.inst.xs1 := DontCare
  cmd.bits.inst.xs2 := DontCare
  cmd.bits.inst.rd := DontCare
  cmd.bits.core_id := to_dispatch
  cmd.bits.payload1 := DontCare

  cmd.bits.payload2 := DontCare // this will be overset
  switch(dState) {
    is(dispatch_idle) {
      when(dispatchQueue.valid) {
        dState := dispatch_A
      }
    }
    is(dispatch_A) {
      cmd.bits.inst.rs1 := GemmCoreCommands.A.asUInt
      cmd.bits.payload2 := coreBuffers(to_dispatch).A
      cmd.valid := true.B
      when(cmd.fire) {
        dState := dispatch_B
      }
    }
    is(dispatch_B) {
      cmd.bits.inst.rs1 := GemmCoreCommands.B.asUInt
      cmd.bits.payload2 := coreBuffers(to_dispatch).B
      cmd.valid := true.B
      when(cmd.fire) {
        dState := dispatch_C
      }
    }
    is(dispatch_C) {
      cmd.bits.inst.rs1 := GemmCoreCommands.C.asUInt
      cmd.bits.payload2 := coreBuffers(to_dispatch).C
      cmd.valid := true.B
      when(cmd.fire) {
        dState := dispatch_sizeAndGo
      }
    }
    is(dispatch_sizeAndGo) {
      cmd.bits.inst.rs1 := GemmCoreCommands.rowAndGo.asUInt
      cmd.bits.inst.xd := true.B
      cmd.bits.payload2 := matSize
      cmd.valid := true.B
      when(cmd.fire) {
        dispatchQueue.ready := true.B // dequeue it!
        dState := dispatch_idle
      }
    }
  }

  composer_response_io.ready := true.B
  when(composer_response_io.valid) {
    val respCore = composer_response_io.bits.data(log2Up(nCores) - 1, 0)
    when(coreBuffers(respCore).progress === progressStoppage) {
      coreIdleBits(respCore) := true.B // otherwise this C is done and the core can be used for other C subsets
    }.otherwise {
      dispatchWire.valid := true.B // put this back into the dispatch queue
    }
    dispatchWire.bits := respCore
    coreBuffers(respCore).progress := coreBuffers(respCore).progress + 1.U
    coreBuffers(respCore).A := coreBuffers(respCore).A + Cat(gp.rowColDim.U, 0.U(2.W))
    coreBuffers(respCore).B := coreBuffers(respCore).B + Cat(matSize, 0.U((log2Up(gp.rowColDim) + 2).W))
  }
}

class withGemmDispatchSystem(gemmParam: GemmParam) extends Config((site, _, up) => {
  case ComposerSystemsKey =>
    val prev = up(ComposerSystemsKey, site)
    prev ++ List(ComposerSystemParams(1,
      name = "GemmDispatch", buildCore = {
        case (constructor, params) =>
          new GemmDispatcher(constructor, gemmParam)(params)
      } , canIssueCoreCommands = true))
})

class GemmWithDispatchConfig(nCores: Int, gp: GemmParam) extends Config(
  new WithGemm(nCores, gp) ++ new withGemmDispatchSystem(gp)
)

class GemmWithFloatDispatchConfig(nCores: Int, gp: GemmParam) extends Config(
  new WithGemmFloat(nCores, gp) ++ new withGemmDispatchSystem(gp)
)