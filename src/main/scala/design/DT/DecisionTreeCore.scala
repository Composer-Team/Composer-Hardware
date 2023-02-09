package design.DT

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import composer.common.{CLog2Up, splitIntoChunks}
import design.DT.DTParams.haveWarned
import design.DT.DecisionTreeCore.shiftOutDistance
import fpnewWrapper._
import freechips.rocketchip.subsystem.ExtMem

object DTParams {
  private var haveWarned = false
}

/**
  * Parameters for Decision Trees
  *
  * @param maxTreeDepth         maximum depth of a tree. All trees need to be stored as full binary trees of this depth in mem
  * @param treeParallelism      how many trees does a decision tree core do at a time (at a maximum)
  * @param indexCompression     In order to increase throughput of BRAMs, store multiple tree indices on one BRAM row
  * @param thresholdCompression Same as index compression except multiple thresholds on one BRAM row
  * @param featureCompression   Ssame as index compression except for multiple features on one BRAM row
  * @param maxNExamples         Maximum number of examples that the dispatcher can count up
  * @param maxNTrees            Maximum number of trees that the dispatcher can support
  * @param maxNFeatures         Maximum number of features that a core can support
  */
case class DTParams(maxTreeDepth: Int,
                    treeParallelism: Int,
                    indexCompression: Int,
                    thresholdCompression: Int,
                    featureCompression: Int,
                    maxNExamples: Int,
                    maxNTrees: Int,
                    maxNFeatures: Int) {
  def getTotalIndexWidth: Int = {
    // round up to nearest pow2. If already pow 2, double size. Unfortunate, but it is the way it is.
    val iwidth = {
      val ti = CLog2Up(maxNFeatures)
      if (isPow2(ti)) {
        if (!haveWarned) {
          println(s"Warning in DecisionTreeCore. Number of bits needed for index within tree is $ti, but we have to" +
            s" add another bit and round to power two to add space for leaf bit. MaxTrees can be increased to ${1 << (ti * 2 - 1)}," +
            s" at basically no memory throughput cost.")
          haveWarned = true
        }
        ti + 1
      } else ti
    }
    val twidth = 1 << CLog2Up(iwidth)
    if (twidth < 8) 8
    else twidth
  }

  def getLongestTxBytes: Int = {
    val threshLength = (1 << maxTreeDepth) * treeParallelism * 4
    val indexes = getTotalIndexWidth / 8 * (1 << maxTreeDepth) * treeParallelism
    val features = 4 * maxNFeatures
    1 << log2Up(Seq(threshLength, indexes, features).max)
  }
}


object DecisionTreeCore {
  def shiftOutDistance(a: UInt, w: Int): UInt = {
    require(w >= 0)
    if (w == 0) a
    else Cat(a, 0.U(w.W))
  }
}

//noinspection DuplicatedCode,ScalaUnnecessaryParentheses
class DecisionTreeCore(composerCoreParams: ComposerConstructor, params: DTParams)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  val (s_idle :: s_secondPayload :: s_FeatureAddress :: s_TreeThresholdAddress :: s_TreeFIDAddress :: s_startNewTree
    :: s_Wait :: s_processing :: s_finishTree :: s_finish :: Nil) = Enum(10)
  val state = RegInit(s_idle)
  io.req.ready := false.B

  require(isPow2(params.indexCompression))
  require(isPow2(params.thresholdCompression))

  val treeIndexWidth = params.getTotalIndexWidth

  val addrWidth = CLog2Up(p(ExtMem).get.master.size)

  val treesToDoMO = Reg(UInt(CLog2Up(params.treeParallelism).W))

  // feature memory
  // we'll process the same item many times if we're doing 10s to thousands of trees so don't reload the feature
  // memory if we don't have to
  val (featureReaderInitIO, featureReader) = getScratchpad("feature")
  val featureAlreadyValid = Reg(Bool())

  val featureAddress = Reg(UInt(addrWidth.W))
  val nFeatureBitWidth = treeIndexWidth - 1
  val nFeatures = Reg(UInt(nFeatureBitWidth.W))

  featureReaderInitIO.request.bits.memAddr := featureAddress
  featureReaderInitIO.request.bits.scAddr := 0.U
  // round this up to a power of two, of course
  featureReaderInitIO.request.bits.len := Cat(nFeatures +& 1.U, 0.U(2.W))
  featureReaderInitIO.request.valid := false.B

  featureReader.readReq.valid := false.B
  featureReader.readReq.bits := DontCare

  /**
    * Tree node consists of a couple pieces of data:
    *   - isLeaf: Bool
    *   - featureSelector: Int
    *   - union(threshold, inference): Float
    *     We force width(featureSelector) to not be a power of two so we can make the highest order bit the isLeaf bit.
    *     If the node IS a leaf, the float member is the resulting inference
    *     If the node is NOT a leaf, then the float member is the threshold s.t. the right sub-tree is chosen iff
    *     features(featureSelector) < threshold
    *     and left-subtree otherwise
    *
    * Although these members have obvious spatial locality to each other, because width(featureSelector) <<< 32b,
    * the node itself will not be a power of 2 size, making it quite a bit harder to manage scratchpads
    * Instead, we split the tree into two arrays:
    *   1. Tuples of (isLeaf, featureSelector) which we pack to size of power of 2 bits
    *      2. thresholds/inference unions
    *      This will ensure that we have only power of 2 lengthed accesses, making scratchpad management quite a bit easier
    *
    * In this code, treeFIDs and treeThresholds correspond to these above arrays. Regardless of the width of
    * featureSelector, isLeaf is the most significant bit
    *
    * The Tree is 1-indexed. The 0th element of the threshold/inference array is (if applicable) the weight of the tree
    * in the Adaboost scheme. The 0th element of the featureSelector array is reserved (currently unused).
    */
  val (treeThresholdIO, treeThresholds) = getScratchpad("treeThreshold")
  val treeThresholdAddress = Reg(UInt(addrWidth.W))

  treeThresholdIO.request.bits.memAddr := treeThresholdAddress
  treeThresholdIO.request.bits.scAddr := 0.U
  treeThresholdIO.request.bits.len := ((1 << params.maxTreeDepth) * 4).U * (treesToDoMO +& 1.U)
  treeThresholdIO.request.valid := false.B

  treeThresholds.readReq.valid := false.B
  treeThresholds.readReq.bits := DontCare

  val (treeFIDIO, treeFIDs) = getScratchpad("featureIDs")
  val treeFIDAddress = Reg(UInt(addrWidth.W))

  treeFIDIO.request.bits.memAddr := treeFIDAddress
  treeFIDIO.request.bits.scAddr := 0.U
  val fid_length = (1 << params.maxTreeDepth) * treeIndexWidth / 8
  println("fid_length: " + fid_length + " " + treeIndexWidth / 8)
  treeFIDIO.request.valid := false.B
  treeFIDIO.request.bits.len := fid_length.U * (treesToDoMO +& 1.U)


  treeFIDs.readReq.valid := false.B
  treeFIDs.readReq.bits := DontCare


  // which tree node are we currently on. Need extra params.maxtreeDepth bits because we store multiple trees in
  // memory at a time
  val treePointerWidth = params.maxTreeDepth
  val currentTreePointer = Reg(UInt(treePointerWidth.W))
  val treeIdx = Reg(UInt(log2Up(params.treeParallelism).W))
  // # trees to do - 1
  val transition_to_processing = Reg(Bool())

  // inference value from current tree
  val treeInference = Reg(UInt(32.W))
  // weighted sum total of the inferences of all processed trees in this command
  val inferenceAccumulator = Reg(UInt(32.W))

  // FPNew floating point unit
  // # stages is absolutely arbitrary and can be changed to pass timing
  // everything works on handshakes so no need to change code anywhere if changing this constant
  val fpunit = Module(new FPUNew(FPFloatFormat.Fp32, lanes = 1, stages = 2, Seq(FPNewOpClass.ADDMUL, FPNewOpClass.NONCOMP), tagWidth = 1))
  // initially set all the fields to DontCare so that stuff like intFormat that isn't used doesn't throw FIRRTL errors
  fpunit.io.req.bits := DontCare
  fpunit.io.flush := false.B

  val fp_in_valid = RegInit(false.B)
  fpunit.io.req.valid := fp_in_valid
  when(fpunit.io.req.fire) {
    fp_in_valid := false.B
  }

  fpunit.io.resp.ready := false.B
  val isLeaf = Reg(Bool())

  // IO tie-offs
  io.resp.bits := DontCare
  io.resp.valid := false.B

  switch(state) {
    is(s_idle) {
      io.req.ready := true.B
      // recieve command
      /**
        * COMMAND FORMAT
        * Cat(p1, p2) = Cat (nFeatures, treeThresholdAddress, featureAddress)
        * rs1 = not zero if starting new feature in this core
        * rs2 = number of trees to do (out of max supported by core)
        */
      when(io.req.fire) {
        val combinedPayloads = Cat(io.req.bits.payload1, io.req.bits.payload2)
        featureAddress := io.req.bits.payload2(addrWidth - 1, 0)
        treeThresholdAddress := io.req.bits.payload1(addrWidth - 1, 0)
        // RS1 stores flag for signaling if this core is working with a feature it's seen before
        //    when in doubt (might be sharing cores with other users), keep low
        featureAlreadyValid := io.req.bits.inst.rs1(0)
        state := s_secondPayload
        currentTreePointer := 0.U
        treeIdx := 0.U
        inferenceAccumulator := 0.U // FP32 0
        treesToDoMO := (if (params.treeParallelism == 1) 0.U else io.req.bits.inst.rs2(CLog2Up(params.treeParallelism) - 1, 0))
      }
    }

    is(s_secondPayload) {
      io.req.ready := true.B
      when(io.req.fire) {
        val combinedPayloads = Cat(io.req.bits.payload1, io.req.bits.payload2)
        treeFIDAddress := combinedPayloads(addrWidth + treeIndexWidth - 1, treeIndexWidth)
        nFeatures := combinedPayloads(treeIndexWidth - 1, 0)
        state := s_FeatureAddress
      }
    }
    is(s_FeatureAddress) {
      // if our feature memory isn't already valid, send signal to load it in
      featureReaderInitIO.request.valid := !featureAlreadyValid
      when(featureReaderInitIO.request.fire || featureAlreadyValid) {
        state := s_TreeThresholdAddress
      }
    }
    is(s_TreeThresholdAddress) {
      // start loading the trees
      treeThresholdIO.request.valid := true.B
      when(treeThresholdIO.request.fire) {
        state := s_TreeFIDAddress
      }
    }
    is(s_TreeFIDAddress) {
      // start loading in the other part of the tree
      treeFIDIO.request.valid := true.B
      when(treeFIDIO.request.fire) {
        state := s_startNewTree
      }
    }
    is(s_startNewTree) {
      // initialization for s_processing
      currentTreePointer := 1.U
      state := s_Wait
    }
    is(s_Wait) {
      // wait for enough of the information to be loaded in that we can start
      val featureLoaded = (shiftOutDistance(featureReaderInitIO.progress, CLog2Up(params.featureCompression)) === params.maxNFeatures.U) || featureAlreadyValid
      val nextTreePtr = Cat(treeIdx +& 1.U, 0.U(params.maxTreeDepth.W))
      val FIds_loaded_enough = shiftOutDistance(treeFIDIO.progress, CLog2Up(params.indexCompression)) >= nextTreePtr
      val thresholds_loaded_enough = shiftOutDistance(treeThresholdIO.progress, CLog2Up(params.thresholdCompression)) >= nextTreePtr
      when(FIds_loaded_enough && thresholds_loaded_enough && featureLoaded) {
        state := s_processing
        transition_to_processing := true.B
        featureAlreadyValid := true.B
      }
    }
    is(s_processing) {
      // start processing
      // stage 1 signal start is transition_to_processing
      //    being in stage 1 implies that the last node we looked at was not a leaf
      treeFIDs.readReq.bits := Cat(treeIdx, currentTreePointer(treePointerWidth - 1, CLog2Up(params.indexCompression)))
      treeFIDs.readReq.valid := transition_to_processing
      when(transition_to_processing) {
        isLeaf := false.B
        transition_to_processing := false.B
      }

      // we store multiple indices together in a row. High-order bits give us the row
      // low order bits give us the position within the row (below)
      val FIDSubIdx = if (params.indexCompression == 1) 0.U else currentTreePointer(CLog2Up(params.indexCompression) - 1, 0)
      val featureSelect = splitIntoChunks(treeFIDs.readRes.bits, treeIndexWidth)(FIDSubIdx)
      val chosenFeature = featureSelect(nFeatureBitWidth - 1, 0)
      val featureSelectHighOrder = chosenFeature(nFeatureBitWidth - 1, CLog2Up(params.featureCompression))
      val featureSelectLowOrder = Reg(UInt(CLog2Up(params.featureCompression).W))
      when(treeFIDs.readRes.valid) {
        isLeaf := featureSelect.head(1).asBool
        featureSelectLowOrder := (if (params.featureCompression == 1) 0.U else chosenFeature(CLog2Up(params.featureCompression) - 1, 0))
      }

      // read from thresholds and features at the same time so that they'll be ready and consumed at the same time
      treeThresholds.readReq.valid := treeFIDs.readRes.valid
      featureReader.readReq.valid := treeFIDs.readRes.valid
      treeThresholds.readReq.bits := Cat(treeIdx, currentTreePointer(treePointerWidth - 1, CLog2Up(params.thresholdCompression)))
      featureReader.readReq.bits := featureSelectHighOrder

      val thresholdSelector = if (params.thresholdCompression == 1) 0.U else currentTreePointer(CLog2Up(params.thresholdCompression) - 1, 0)
      val threshold = splitIntoChunks(treeThresholds.readRes.bits, 32)(thresholdSelector)
      val feature = splitIntoChunks(featureReader.readRes.bits, 32)(featureSelectLowOrder)

      assert(!(treeThresholds.readRes.valid ^ featureReader.readRes.valid),
        "threshold and feature have to be available at the same time")

      when(isLeaf && treeThresholds.readRes.valid) {
        treeInference := threshold
        state := s_finishTree
        treeThresholds.readReq.valid := true.B
        treeThresholds.readReq.bits := Cat(treeIdx, 0.U((params.maxTreeDepth - CLog2Up(params.thresholdCompression)).W))
      }
      fp_in_valid := treeThresholds.readRes.valid && !isLeaf
      fpunit.io.req.bits.operands(0) := feature
      fpunit.io.req.bits.operands(1) := threshold
      fpunit.io.req.bits.op := FPOperation.CMP
      fpunit.io.req.bits.roundingMode := FPRoundingMode.RTZ // op[0] < op[1] - README.md:109
      fpunit.io.req.bits.opModifier := 0.U
      fpunit.io.req.bits.srcFormat := FPFloatFormat.Fp32
      fpunit.io.resp.ready := true.B
      // if we made it here, implied not a leaf node
      when(fpunit.io.resp.fire) {
        val nextPointer = Cat(currentTreePointer.tail(1), fpunit.io.resp.bits.result(0))
        transition_to_processing := true.B
        currentTreePointer := nextPointer
      }
    }
    is(s_finishTree) {
      fpunit.io.req.valid := treeThresholds.readRes.valid
      fpunit.io.req.bits.operands(0) := splitIntoChunks(treeThresholds.readRes.bits, 32)(0)
      fpunit.io.req.bits.operands(1) := treeInference
      fpunit.io.req.bits.operands(2) := inferenceAccumulator
      fpunit.io.req.bits.op := FPOperation.FMADD
      fpunit.io.req.bits.opModifier := 0.U
      fpunit.io.req.bits.srcFormat := FPFloatFormat.Fp32
      // op0 * op1 + op2

      // round to nearest (tie to zero)
      fpunit.io.req.bits.roundingMode := FPRoundingMode.RNE
      fpunit.io.resp.ready := true.B
      when(fpunit.io.resp.fire) {
        inferenceAccumulator := fpunit.io.resp.bits.result
        if (params.treeParallelism == 1) {
          state := s_finish
        } else {
          when(treeIdx === treesToDoMO) {
            state := s_finish
          }.otherwise {
            state := s_startNewTree
            treeIdx := treeIdx + 1.U
          }
        }
      }
    }
    is(s_finish) {
      io.resp.bits.data := Cat(getCoreID().U(p(CoreIDLengthKey).W), inferenceAccumulator)
      require(io.resp.bits.data.getWidth >= 32 + p(CoreIDLengthKey))
      io.resp.valid := true.B
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }
}