package design.DT

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import fpnewWrapper.fpnew._
import freechips.rocketchip.subsystem.ExtMem

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
                    maxNFeatures: Int)


object DecisionTreeCore {
  private var have_warned = false

  def getTotalIndexWidth(params: DTParams): Int = {
    // round up to nearest pow2. If already pow 2, double size. Unfortunate, but it is the way it is.
    val iwidth = {
      val ti = log2Up(params.maxNFeatures)
      if (isPow2(ti)) {
        if (!have_warned) {
          println(s"Warning in DecisionTreeCore. Number of bits needed for index within tree is $ti, but we have to" +
            s" add another bit and round to power two to add space for leaf bit. MaxTrees can be increased to ${1 << (ti * 2 - 1)}," +
            s" at basically no memory throughput cost.")
          have_warned = true
        }
        ti + 1
      } else ti
    }
    val twidth = 1 << log2Up(iwidth)
    if (twidth < 8) 8
    else twidth
  }
}

//noinspection DuplicatedCode
class DecisionTreeCore(composerCoreParams: ComposerConstructor, params: DTParams)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  def splitIntoChunks(a: UInt, sz: Int): Seq[UInt] = {
    require(a.getWidth % sz == 0, s"Can't split bitwidth ${a.getWidth} into chunks of $sz")

    def rec(a: UInt, acc: List[UInt] = List.empty): List[UInt] = {
      if (a.getWidth == 0) acc
      else a(sz - 1, 0) :: acc
    }

    rec(a)
  }

  val s_idle :: s_secondPayload :: s_FeatureAddress :: s_TreeThresholdAddress :: s_TreeFIDAddress :: s_startNewTree :: s_Wait :: s_processing :: s_finishTree :: s_finish :: Nil = Enum(10)
  val state = RegInit(s_idle)
  io.req.ready := false.B

  require(isPow2(params.indexCompression))
  require(isPow2(params.thresholdCompression))

  val addrWidth = log2Up(p(ExtMem).get.master.size)

  // feature memory
  // we'll process the same item many times if we're doing 10s to thousands of trees so don't reload the feature
  // memory if we don't have to
  val (featureReaderInitIO, featureReader) = getScratchpad("feature")
  val featureAlreadyValid = Reg(Bool())

  val featureAddress = Reg(UInt(addrWidth.W))
  val nFeatureBitWidth = log2Up(params.maxNFeatures)
  val nFeatures = Reg(UInt(nFeatureBitWidth.W))

  featureReaderInitIO.get.request.bits.memAddr := featureAddress
  featureReaderInitIO.get.request.bits.scAddr := 0.U
  // round this up to a power of two, of course
  featureReaderInitIO.get.request.bits.len := Cat(nFeatures +& 1.U, 0.U(2.W))
  featureReaderInitIO.get.request.valid := false.B

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

  treeThresholdIO.get.request.bits.memAddr := treeThresholdAddress
  treeThresholdIO.get.request.bits.scAddr := 0.U
  // 4 bytes for each threshold
  treeThresholdIO.get.request.bits.len := (Math.pow(2, params.maxTreeDepth).toInt * params.treeParallelism * 4).U
  treeThresholdIO.get.request.valid := false.B

  val (treeFIDIO, treeFIDs) = getScratchpad("featureIDs")
  val treeFIDAddress = Reg(UInt(addrWidth.W))

  treeFIDIO.get.request.bits.memAddr := treeFIDAddress
  treeFIDIO.get.request.bits.scAddr := 0.U
  treeFIDIO.get.request.bits.len := (Math.pow(2, params.maxTreeDepth).toInt * params.treeParallelism * 2).U
  treeFIDIO.get.request.valid := false.B

  // which tree node are we currently on. Need extra params.maxtreeDepth bits because we store multiple trees in
  // memory at a time
  val treePointerWidth = params.maxTreeDepth + log2Up(params.treeParallelism)
  val currentTreePointer = Reg(UInt(treePointerWidth.W))
  val treePointers = VecInit(Seq.tabulate(params.treeParallelism)(treeN => (treeN * (1 << params.maxTreeDepth)).U))
  val treeIdx = currentTreePointer.head(log2Up(params.treeParallelism))
  // # trees to do - 1
  val treesToDoMO = Reg(UInt(log2Up(params.treeParallelism).W))
  val beginning = Reg(Bool())
  val transition_to_processing = Reg(Bool())

  // inference value from current tree
  val treeInference = Reg(UInt(32.W))
  // weighted sum total of the inferences of all processed trees in this command
  val inferenceAccumulator = Reg(UInt(32.W))

  // FPNew floating point unit
  // # stages is absolutely arbitrary and can be changed to pass timing
  // everything works on handshakes so no need to change code anywhere if changing this constant
  val fpunit = Module(new FPUNew(FPNewFType.FullPrecision, lanes = 1, stages = 2))
  fpunit.io.flush := false.B
  fpunit.io.req.valid := false.B
  fpunit.io.resp.ready := false.B
  fpunit.io.req.bits := DontCare
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
        featureAddress := combinedPayloads(addrWidth - 1, 0)
        treeThresholdAddress := combinedPayloads(addrWidth * 2 - 1, addrWidth)
        // RS1 stores flag for signaling if this core is working with a feature it's seen before
        //    when in doubt (might be sharing cores with other users), keep low
        featureAlreadyValid := io.req.bits.inst.rs1(0)
        state := s_secondPayload
        currentTreePointer := 0.U
        beginning := true.B
        inferenceAccumulator := 0.U // FP32 0
        treesToDoMO := io.req.bits.inst.rs2(log2Up(params.treeParallelism) - 1, 0)
      }
    }

    is(s_secondPayload) {
      io.req.ready := true.B
      when(io.req.fire) {
        val combinedPayloads = Cat(io.req.bits.payload1, io.req.bits.payload2)
        treeFIDAddress := treeThresholdAddress + {
          val allocSize = params.maxNTrees * 4
          if ((allocSize & 0xFFF) != 0) // not page aligned, make it so
            (allocSize + (1 << 12)) & ~0xFFF
          else allocSize
        }.U
        nFeatures := combinedPayloads(nFeatureBitWidth - 1, 0)
        state := s_FeatureAddress
      }
    }
    is(s_FeatureAddress) {
      // if our feature memory isn't already valid, send signal to load it in
      featureReaderInitIO.get.request.valid := !featureAlreadyValid
      when(featureReaderInitIO.get.request.fire || featureAlreadyValid) {
        state := s_TreeThresholdAddress
        featureAlreadyValid := true.B
      }
    }
    is(s_TreeThresholdAddress) {
      // start loading the trees
      treeThresholdIO.get.request.valid := true.B
      when(treeThresholdIO.get.request.fire) {
        state := s_TreeFIDAddress
      }
    }
    is(s_TreeFIDAddress) {
      // start loading in the other part of the tree
      treeFIDIO.get.request.valid := true.B
      when(treeFIDIO.get.request.fire) {
        state := s_startNewTree
      }
    }
    is(s_startNewTree) {
      // initialization for s_processing
      when(beginning) {
        currentTreePointer := treePointers(0) + 1.U
        beginning := false.B
      }.otherwise {
        currentTreePointer := treePointers(treeIdx + 1.U) + 1.U
      }
      state := s_Wait
    }
    is(s_Wait) {
      // wait for enough of the information to be loaded in that we can start
      def passes(progress: UInt, isDone: Bool): Bool = {
        if (params.treeParallelism == 1) {
          isDone
        } else {
          val flag = Wire(Bool())
          when(treeIdx < (params.treeParallelism - 1).U) {
            flag := progress >= treePointers(treeIdx + 1.U)
          }.otherwise {
            flag := isDone
          }
          flag
        }
      }

      val featureLoaded = featureReaderInitIO.get.request.ready
      val treeLoaded = passes(Cat(treeFIDIO.get.progress, 0.U(log2Up(params.indexCompression).W)), treeFIDIO.get.request.ready) &&
        passes(Cat(treeThresholdIO.get.progress, 0.U(log2Up(params.thresholdCompression).W)), treeThresholdIO.get.request.ready)
      when(treeLoaded && featureLoaded) {
        state := s_processing
        transition_to_processing := true.B
      }
    }
    is(s_processing) {
      // start processing
      // stage 1 signal start is transition_to_processing
      //    being in stage 1 implies that the last node we looked at was not a leaf
      treeFIDs.readReq.bits := currentTreePointer(treePointerWidth - 1, log2Up(params.indexCompression))
      treeFIDs.readReq.valid := transition_to_processing
      when(transition_to_processing) {
        isLeaf := false.B
        transition_to_processing := false.B
      }

      // we store multiple indices together in a row. High-order bits give us the row
      // low order bits give us the position within the row (below)
      val FIDSubIdx = currentTreePointer(log2Up(params.indexCompression) - 1, 0)
      val featureSelect = VecInit(splitIntoChunks(treeFIDs.readRes.bits, DecisionTreeCore.getTotalIndexWidth(params)))(FIDSubIdx)
      val chosenFeature = featureSelect(nFeatureBitWidth - 1, 0)
      when(featureSelect.head(1).asBool) {
        isLeaf := true.B
      }
      val featureSelectHighOrder = chosenFeature(nFeatureBitWidth - 1, log2Up(params.featureCompression))
      val featureSelectLowOrder = Reg(UInt(log2Up(params.featureCompression).W))
      when(treeFIDs.readRes.valid) {
        featureSelectLowOrder := chosenFeature(log2Up(params.featureCompression) - 1, 0)
      }

      // read from thresholds and features at the same time so that they'll be ready and consumed at the same time
      treeThresholds.readReq.valid := treeFIDs.readRes.valid
      featureReader.readReq.valid := treeFIDs.readRes.valid
      treeThresholds.readReq.bits := currentTreePointer(treePointerWidth - 1, log2Up(params.thresholdCompression))
      featureReader.readReq.bits := featureSelectHighOrder
      val threshold = VecInit(splitIntoChunks(treeThresholds.readRes.bits, 32))(currentTreePointer(log2Up(params.thresholdCompression) - 1, 0))
      val feature = VecInit(splitIntoChunks(featureReader.readRes.bits, 32))(featureSelectLowOrder)

      assert(!(treeThresholds.readRes.valid ^ featureReader.readRes.valid),
        "threshold and feature have to be available at the same time")

      when(isLeaf && treeThresholds.readRes.valid) {
        treeInference := threshold
        state := s_finishTree
        treeThresholds.readReq.valid := true.B
        treeThresholds.readReq.bits := Cat(treeIdx, 0.U(params.maxTreeDepth.W))
      }
      fpunit.io.req.valid := treeThresholds.readRes.valid && !isLeaf
      fpunit.io.req.bits.op := FPOperation.CMP
      fpunit.io.req.bits.roundingMode := FPRoundingMode.RTZ // op[0] < op[1] - README.md:109
      fpunit.io.req.bits.operands(0) := feature
      fpunit.io.req.bits.operands(1) := threshold
      fpunit.io.req.bits.opModifier := 0.U
      fpunit.io.req.bits.srcFormat := FPFloatFormat.Fp32
      // all other operands are DontCare
      when(fpunit.io.req.valid) {
        assert(fpunit.io.req.ready, "sanity")
      }
      fpunit.io.resp.ready := true.B
      // if we made it here, implied not a leaf node
      when(fpunit.io.resp.fire) {
        val nextPointer = Cat(currentTreePointer.tail(1), fpunit.io.resp.bits.result(0))
        transition_to_processing := true.B
        currentTreePointer := nextPointer
      }
    }
    is(s_finishTree) {
      when(treeThresholds.readRes.valid) {
        assert(fpunit.io.req.ready, "sanity 2")
      }
      fpunit.io.req.valid := treeThresholds.readRes.valid
      fpunit.io.req.bits.op := FPOperation.FMADD
      fpunit.io.req.bits.opModifier := 0.U
      fpunit.io.req.bits.srcFormat := FPFloatFormat.Fp32
      // op0 * op1 + op2
      fpunit.io.req.bits.operands(0) := VecInit(splitIntoChunks(treeThresholds.readRes.bits, 32))(0)
      fpunit.io.req.bits.operands(1) := treeInference
      fpunit.io.req.bits.operands(2) := inferenceAccumulator
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
          }
        }
      }
    }
    is(s_finish) {
      require(io.resp.bits.data.getWidth >= 32)
      val rdBits = io.resp.bits.rd.getWidth
      val cIdBits = p(CoreIDLengthKey)
      val coreID = composerCoreParams.composerCoreParams.core_id
      val returnPayload = Cat(coreID.U, inferenceAccumulator)
      if (returnPayload.getWidth <= io.resp.bits.data.getWidth) {
        io.resp.bits.data := returnPayload
      } else {
        require(returnPayload.getWidth <= io.resp.bits.data.getWidth + io.resp.bits.rd.getWidth)
        io.resp.bits.data := returnPayload(io.resp.bits.data.getWidth - 1, 0)
        io.resp.bits.rd := returnPayload(returnPayload.getWidth - 1, io.resp.bits.data.getWidth)
      }
      if (cIdBits <= rdBits) {
        io.resp.bits.rd := coreID.U
        io.resp.bits.data := inferenceAccumulator
      } else {
        io.resp.bits.rd := coreID.U(rdBits - 1, 0)
        io.resp.bits.data := Cat(coreID.U(cIdBits - 1, rdBits), inferenceAccumulator)
        require(io.resp.bits.data.getWidth >= inferenceAccumulator.getWidth + cIdBits - rdBits)
      }
      io.resp.valid := true.B
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }
}