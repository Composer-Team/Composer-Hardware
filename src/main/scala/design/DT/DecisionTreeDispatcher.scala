package design.DT

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.{ComposerConstructor, ComposerCore, ComposerFunc, ComposerSystemsKey, CoreIDLengthKey, CppGeneration}
import composer.common.{CLog2Up, ComposerRoccCommand}
import fpnewWrapper.fpnew.{FPFloatFormat, FPNewFType, FPOperation, FPRoundingMode, FPUNew}
import freechips.rocketchip.subsystem.ExtMem

class DecisionTreeDispatcher(composerCoreParams: ComposerConstructor, params: DTParams)(implicit p: Parameters)
  extends ComposerCore(composerCoreParams) {
  // we can make sure that some of these params of our choosing are available for software to use as well
  val DTCoreSystemParams = p(ComposerSystemsKey).filter(_.name == "DTCores")(0)

  // send commands to other systems through here
  val comm = composer_command_io.get
  comm.valid := false.B
  comm.bits := DontCare

  val s_idle :: s_addr :: s_active :: s_first :: s_wait :: s_commit :: s_finishing :: Nil = Enum(7)
  val state = RegInit(s_idle)
  io.busy := state === s_idle
  io.req.ready := state === s_idle
  io.resp.valid := false.B

  val addressBits = CLog2Up(p(ExtMem).get.master.size)
  val exampleIdxBits = CLog2Up(params.maxNExamples)
  val nTreeBits = CLog2Up(params.maxNTrees)
  val pageBoundary = 12
  val treeIndexBits = DecisionTreeCore.getTotalIndexWidth(params)

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
  val nFeatureIdxBits = CLog2Up(params.maxNFeatures)
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

  val fpUnits = Module(new FPUNew(FPNewFType.FullPrecision, 1, 2))
  // recieve responses for those commands back through here
  val resp = composer_response_io.get
  resp.ready := true.B
  fpUnits.io.req.valid := resp.valid
  val responsePayload = Cat(resp.bits.rd, resp.bits.data)
  val respondingCore = responsePayload(31 + p(CoreIDLengthKey), 32)
  when(resp.fire) {
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
  fpUnits.io.req.bits.operands(2) := resp.bits.data(31, 0)
  fpUnits.io.req.bits.operands(0) := DontCare
  fpUnits.io.flush := false.B

  def loadPayloadIntoRocc(payload: UInt, rocc: ComposerRoccCommand): Unit = {
    val plen = payload.getWidth
    val p1len = comm.bits.payload1.getWidth
    val p2len = comm.bits.payload2.getWidth
    rocc.payload2 := (if (plen < p2len) Cat(0.U((p2len - plen).W), payload) else payload(p2len - 1, 0))
    val plenLeft = plen - p2len
    rocc.payload1 := {
      if (plenLeft <= 0) DontCare
      else if (plenLeft <= p1len) Cat(0.U((p1len - plenLeft).W), payload(p1len + plenLeft - 1, p1len))
      else throw new Exception(s"Not enough payload! Available: ${p1len + p2len}. Required: $plen")
    }

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
      treeIndexAddr := Cat(treeThresholdAddrSave(addressBits - 1, pageBoundary) + 1.U, 0.U(pageBoundary.W))
      inferenceWriterReqIO.valid := true.B
      when(inferenceWriterReqIO.fire) {
        state := s_active
        hasCoreSeenFeature.foreach(_ := false.B)
      }
    }
    is(s_active) {
      val coreChoice = PriorityEncoder(coreBusyBits.map(!_))

      val canIssue = coreBusyBits.map(!_).reduce(_ || _)
      when(canIssue) {
        allocatedCore := coreChoice
      }
      comm.bits.core_id := coreChoice
      val payload = Cat(treeThresholdAddr, featureAddr)
      loadPayloadIntoRocc(payload, comm.bits)
      comm.bits.inst.rs1 := Cat(0.U((comm.bits.inst.rs1.getWidth - 1).W), hasCoreSeenFeature(coreChoice))
      comm.bits.inst.rs2 := Mux(numTrees >= (params.treeParallelism - 1).U, (params.treeParallelism - 1).U, numTrees(CLog2Up(params.treeParallelism - 1) - 1, 0))
      comm.bits.inst.system_id := getSystemID("DTCores")
      comm.bits.inst.xd := true.B
      comm.bits.inst.funct := ComposerFunc.START.U
      comm.bits.inst.xs1 := DontCare
      comm.bits.inst.xs2 := DontCare
      comm.valid := canIssue
      when(comm.fire) {
        coreBusyBits(coreChoice) := true.B
        hasCoreSeenFeature(coreChoice) := true.B
        treeIndexAddr := treeIndexAddr + ((DecisionTreeCore.getTotalIndexWidth(params) / 8) * params.treeParallelism * (1 << params.maxTreeDepth)).U
        treeThresholdAddr := treeThresholdAddr + (4 * params.treeParallelism * (1 << params.maxTreeDepth)).U
        state := s_first
      }
    }
    is(s_first) {
      comm.valid := true.B
      comm.bits.inst.system_id := getSystemID("DTCores")
      comm.bits.core_id := allocatedCore
      comm.bits.inst.xd := false.B
      loadPayloadIntoRocc(numFeatures, comm.bits)
      when(comm.fire) {
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
            else Cat(numFeatures, 0.U(CLog2Up(DecisionTreeCore.getTotalIndexWidth(params) / 8).W)))
          // start trees over
          treeThresholdAddr := treeThresholdAddrSave
          treeIndexAddr := Cat(treeThresholdAddrSave(addressBits - 1, pageBoundary) + 1.U, 0.U(pageBoundary.W))

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
