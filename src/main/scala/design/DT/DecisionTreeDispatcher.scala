package design.DT

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.RoccHelpers.ComposerFunc
import composer.{ComposerConstructor, ComposerCore, ComposerSystemsKey, CoreIDLengthKey, CppGeneration}
import composer.common.{CLog2Up, ComposerRoccCommand}
import fpnewWrapper.{FPFloatFormat, FPNewOpClass, FPOperation, FPRoundingMode, FPUNew}
import freechips.rocketchip.subsystem.ExtMem

class DecisionTreeDispatcher(composerCoreParams: ComposerConstructor, params: DTParams)(implicit p: Parameters)
  extends ComposerCore(composerCoreParams) {
  // we can make sure that some of these params of our choosing are available for software to use as well
  val DTCoreSystemParams = p(ComposerSystemsKey).filter(_.name == "DTCores")(0)

  // send commands to other systems through here
  composer_command_io.valid := false.B
  composer_command_io.bits := DontCare

  val s_idle :: s_addr :: s_active :: s_first :: s_wait :: s_commit :: s_finishing :: Nil = Enum(7)
  val state = RegInit(s_idle)
  io.busy := state === s_idle
  io.req.ready := state === s_idle
  io.resp.valid := false.B

  val addressBits = CLog2Up(p(ExtMem).get.master.size)
  val exampleIdxBits = CLog2Up(params.maxNExamples)
  val nTreeBits = CLog2Up(params.maxNTrees)
  val treeIndexBits = params.getTotalIndexWidth

  /**
    * Tree arrays are stored in memory on 4KB page aligned boundaries following each other
    * First is thresholds, second is indicies
    */
  val treeIndexAddr = Reg(UInt(addressBits.W))
  val treeThresholdAddr = Reg(UInt(addressBits.W))
  val treeThresholdAddrSave = Reg(UInt(addressBits.W))
  val featureAddr = Reg(UInt(addressBits.W))
  // NOTE: IF YOU WANT TO DO N EXAMPLES, SEND IN N-1. ex. 1 example, send 0. 14 examples, send 13
  val numExamples = Reg(UInt(exampleIdxBits.W))
  val numTrees = Reg(UInt(nTreeBits.W))
  val numTreesSave = Reg(UInt(nTreeBits.W))
  val outputAddr = Reg(UInt(addressBits.W))
  val nFeatureIdxBits = params.getTotalIndexWidth
  val numFeatures = Reg(UInt(nFeatureIdxBits.W))

  val inferenceWriterLists = getWriterModules(name = "OutputWriter", useSoftwareAddressing = false, dataBytes = 4)
  val inferenceWriter = inferenceWriterLists._2(0)
  inferenceWriter.finishEarly := false.B
  val inferenceWriterReqIO = inferenceWriterLists._1(0)
  inferenceWriterReqIO.bits.addr := outputAddr
  inferenceWriterReqIO.bits.len := Cat(numExamples +& 1.U, 0.U(2.W)) // numExamples * 4 bytes
  inferenceWriterReqIO.valid := false.B
  inferenceWriter.data.valid := false.B
  val inferenceAccumulator = Reg(UInt(32.W))
  inferenceWriter.data.bits := inferenceAccumulator

  val coreBusyBits = RegInit(VecInit(Seq.fill(DTCoreSystemParams.nCores)(false.B)))
  val hasCoreSeenFeature = Reg(Vec(DTCoreSystemParams.nCores, Bool()))
  val allocatedCore = Reg(UInt(CLog2Up(DTCoreSystemParams.nCores).W))

  val fpUnits = Module(new FPUNew(FPFloatFormat.Fp32, 1, 2, Seq(FPNewOpClass.ADDMUL), tagWidth = 1))
  // recieve responses for those commands back through here
  composer_response_io.ready := true.B
  fpUnits.io.req.valid := composer_response_io.valid
  val respondingCore = composer_response_io.bits.data(31 + p(CoreIDLengthKey), 32)
  when(composer_response_io.fire) {
    coreBusyBits(respondingCore) := false.B
  }
  fpUnits.io.resp.ready := true.B
  fpUnits.io.req.bits.roundingMode := FPRoundingMode.RNE
  fpUnits.io.req.bits.op := FPOperation.ADD
  fpUnits.io.req.bits.opModifier := 0.U
  fpUnits.io.req.bits.srcFormat := FPFloatFormat.Fp32
  fpUnits.io.req.bits.intFormat := DontCare
  fpUnits.io.req.bits.dstFormat := DontCare
  fpUnits.io.req.bits.operands(1) := inferenceAccumulator
  fpUnits.io.req.bits.operands(2) := composer_response_io.bits.data(31, 0)
  fpUnits.io.req.bits.operands(0) := DontCare
  fpUnits.io.flush := false.B
  fpUnits.io.req.bits.tag := DontCare
  when (fpUnits.io.resp.valid) {
    inferenceAccumulator := fpUnits.io.resp.bits.result
  }

  def loadPayloadIntoRocc(payload: UInt, rocc: ComposerRoccCommand): Unit = {
    val plen = payload.getWidth
    val p1len = composer_command_io.bits.payload1.getWidth
    val p2len = composer_command_io.bits.payload2.getWidth
    rocc.payload2 := (if (plen < p2len) Cat(0.U((p2len - plen).W), payload) else payload(p2len - 1, 0))
    val plenLeft = plen - p2len
    rocc.payload1 := {
      if (plenLeft <= 0) DontCare
      else if (plenLeft <= p1len) Cat(0.U((p1len - plenLeft).W), payload(p1len + plenLeft - 1, p1len))
      else throw new Exception(s"Not enough payload! Available: ${p1len + p2len}. Required: $plen")
    }
  }

  def pageUp(a: Int): Int = {
    if ((a & 0xFFF) != 0) {
      (a + (1 << 12)) & 0xFFF
    } else a
  }

  switch(state) {
    is(s_idle) {
      io.req.ready := true.B
      when(io.req.fire) {
        // rs1 == 0 : Cat (numFeatures, treeAddr, featureAddr) AND GO
        // rs1 == 1 : Cat (outputAddr, nTrees, nExamples) - do not start
        val concatPayload = Cat(io.req.bits.payload1, io.req.bits.payload2)
        when(io.req.bits.inst.rs1 === 0.U) {
          featureAddr := concatPayload(addressBits - 1, 0)
          val treeAddr = concatPayload(addressBits * 2 - 1, addressBits)
          treeThresholdAddr := treeAddr
          treeIndexAddr := treeAddr + pageUp((1 << params.maxTreeDepth) * params.maxNTrees * 4).U
          treeThresholdAddrSave := treeAddr
          numFeatures := concatPayload(addressBits * 2 + nFeatureIdxBits - 1, addressBits * 2)
          state := s_addr
        }.otherwise {
          numExamples := concatPayload(exampleIdxBits - 1, 0)
          numTrees := concatPayload(nTreeBits + exampleIdxBits - 1, exampleIdxBits)
          numTreesSave := concatPayload(nTreeBits + exampleIdxBits - 1, exampleIdxBits)
          outputAddr := concatPayload(addressBits + nTreeBits + exampleIdxBits - 1, nTreeBits + exampleIdxBits)
        }
        CppGeneration.addUserCppDefinition(Seq(("int32_t", "maxNTrees", params.maxNTrees),
          ("int32_t", "maxNFeatures", params.maxNFeatures),
          ("int32_t", "maxTreeDepth", params.maxTreeDepth),
          ("int32_t", "maxNExamples", params.maxNExamples),
          ("uint32_t", "exampleBits", exampleIdxBits),
          ("uint32_t", "nTreeBits", nTreeBits),
          ("uint32_t", "nFeatureIdxBits", nFeatureIdxBits)
        ))
      }
    }
    is(s_addr) {
      // indices live on page boundary right after thresholds
      inferenceWriterReqIO.valid := true.B
      when(inferenceWriterReqIO.fire) {
        state := s_active
        hasCoreSeenFeature.foreach(_ := false.B)
      }
      inferenceAccumulator := 0.U
    }
    is(s_active) {
      val coreChoice = PriorityEncoder(coreBusyBits.map(!_))

      val canIssue = coreBusyBits.map(!_).reduce(_ || _)
      when(canIssue) {
        allocatedCore := coreChoice
      }
      composer_command_io.bits.core_id := coreChoice
      composer_command_io.bits.payload2 := featureAddr
      composer_command_io.bits.payload1 := treeThresholdAddr
//      val payload = Cat(treeThresholdAddr, featureAddr)
//      loadPayloadIntoRocc(payload, composer_command_io.bits)
      composer_command_io.bits.inst.rs1 := Cat(0.U((composer_command_io.bits.inst.rs1.getWidth - 1).W), hasCoreSeenFeature(coreChoice))
      if (params.treeParallelism > 1) {
        if (params.treeParallelism > params.maxNTrees) {
          composer_command_io.bits.inst.rs2 := (params.maxNTrees-1).U
        } else
          composer_command_io.bits.inst.rs2 := Mux(numTrees >= (params.treeParallelism - 1).U, (params.treeParallelism - 1).U, numTrees(CLog2Up(params.treeParallelism) - 1, 0))
      } else {
        composer_command_io.bits.inst.rs2 := 0.U
      }
      composer_command_io.bits.inst.system_id := getSystemID("DTCores")
      composer_command_io.bits.inst.xd := true.B
      composer_command_io.bits.inst.funct := ComposerFunc.START.U
      composer_command_io.bits.inst.xs1 := DontCare
      composer_command_io.bits.inst.xs2 := DontCare
      composer_command_io.valid := canIssue
      when(composer_command_io.fire) {
        coreBusyBits(coreChoice) := true.B
        hasCoreSeenFeature(coreChoice) := true.B
        treeThresholdAddr := treeThresholdAddr + (4 * params.treeParallelism * (1 << params.maxTreeDepth)).U
        state := s_first
      }
    }
    is(s_first) {
      composer_command_io.valid := true.B
      composer_command_io.bits.inst.system_id := getSystemID("DTCores")
      composer_command_io.bits.core_id := allocatedCore
      composer_command_io.bits.inst.xd := false.B
      loadPayloadIntoRocc(Cat(treeIndexAddr, numFeatures), composer_command_io.bits)
      when(composer_command_io.fire) {
        val tisize = (params.getTotalIndexWidth / 8) * params.treeParallelism * (1 << params.maxTreeDepth)
        treeIndexAddr := treeIndexAddr + tisize.U

        when(numTrees <= (params.treeParallelism - 1).U) {
          state := s_wait
        }.otherwise {
          state := s_active
        }
        numTrees := numTrees - params.treeParallelism.U
      }
    }
    is(s_wait) {
      when(!coreBusyBits.reduce(_ || _) && fpUnits.io.resp.fire) {
        state := s_commit
      }
    }
    is(s_commit) {
      inferenceWriter.data.valid := true.B
      inferenceWriter.data.bits := inferenceAccumulator
      when(inferenceWriter.data.ready) {
        numExamples := numExamples - 1.U
        when(numExamples === 0.U) {
          state := s_finishing
        }.otherwise {
          state := s_active
          hasCoreSeenFeature.foreach(_ := false.B)
          // advance features
          featureAddr := featureAddr +
            (if (treeIndexBits / 8 == 1) numFeatures
            else Cat(numFeatures, 0.U(CLog2Up(params.getTotalIndexWidth / 8).W)))
          // start trees over
          treeThresholdAddr := treeThresholdAddrSave
          treeIndexAddr := treeThresholdAddrSave + pageUp((1 << params.maxTreeDepth) * params.maxNTrees * 4).U
          inferenceAccumulator := 0.U

        }
      }
    }
    is(s_finishing) {
      inferenceWriter.finishEarly := true.B
      io.resp.valid := inferenceWriter.channelIdle
      io.resp.bits.data := DontCare
      io.resp.bits.rd := 0.U
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }
}
