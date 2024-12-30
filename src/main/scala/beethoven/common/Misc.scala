package beethoven.common

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.NodeHandle

import scala.annotation.tailrec
import scala.collection.SeqMap

/**
 *
 */
trait hasAccessibleUserSubRegions {
  val reservedNames: Seq[String]

  val elements: SeqMap[String, Data]

  def sortedElements: Seq[(String, Data)]

  private[beethoven] def realDatas: Seq[(String, Data)] = sortedElements.filter(ele => !reservedNames.contains(ele._1))

  private[beethoven] def realElements: Seq[(String, Data)] = realDatas.toSeq.map(a => (a._1, a._2))

  private[beethoven] def fieldSubranges: Seq[(String, (Int, Int))] = {
    val eles = realElements
    val scan = eles.map(_._2.getWidth).scan(0)(_ + _).tail
    eles zip scan map { case (e, top) => (e._1, (top - 1, top - e._2.getWidth)) }
  }

  private def do_:=(other: UInt) = {
    this.fieldSubranges.foreach { case (ele_name, (high, low)) =>
      elements(ele_name) := other(high, low)
    }
  }
}

object hasAccessibleUserSubRegions {
  def apply[T <: hasAccessibleUserSubRegions with Bundle](in: UInt, gen: T): T = {
    val a = Wire(gen)
    a.fieldSubranges.foreach { case (ele_name, (high, low)) =>
      val ele = a.elements(ele_name)
      ele match {
        case regions: hasAccessibleUserSubRegions =>
          regions.do_:=(in(high, low))
        case _ => a.elements(ele_name) := in(high, low)
      }
    }
    a
  }
}

object CLog2Up {
  def apply(a: BigInt): Int = {
    if (a == 1) 0
    else log2Up(a)
  }
}

object splitIntoChunks {
  def apply(a: UInt, sz: Int, withName: Option[String] = None): Vec[UInt] = {
    require(a.getWidth % sz == 0, s"Can't split bitwidth ${a.getWidth} into chunks of $sz")
    val nDivs = a.getWidth / sz
    val wrs = Seq.tabulate(nDivs) { idx =>
      val start = idx * sz
      val end = (idx + 1) * sz - 1
      a(end, start)
    }
    val myVec = VecInit(wrs)
    withName match {
      case Some(a) => myVec.suggestName(a)
      case None => ;
    }
    myVec
  }
}

object applyToChunks {
  def apply(a: UInt, sz: Int, op: UInt => UInt): UInt = {
    val subs = splitIntoChunks(a, sz)
    Cat(subs.reverse.map(op))
  }
}

object Misc {
  def left_assign[T <: Data](tup: (T, T)): Unit = tup._1 := tup._2


  /**
   * given a list of terms and a list of dependencies, return a topological sort of the terms
   * source: https://en.wikipedia.org/wiki/Topological_sorting
   * Kahn's algorithm
   *
   * @param ins          the terms
   * @param dependencies tuples of (a, b term), a depends on b
   * @return
   */
  @tailrec
  def topological_sort_depends(ins: List[String], dependencies: List[(String, String)], L: List[String] = List.empty): List[String] = {
    val S = ins.filterNot(a => dependencies.exists(_._1 == a))
    assert(S.nonEmpty, s"Cycle detected in dependencies: $dependencies")
    val Sp = ins.filter(a => dependencies.exists(_._1 == a))
    val Lp = L ++ S
    val deps = dependencies.filterNot(a => S.contains(a._2))
    if (Sp.isEmpty) {
      Lp
    } else {
      topological_sort_depends(Sp, deps, Lp)
    }
  }

  def round2Pow2(x: Int): Int = 1 << log2Up(x)

  def multByIntPow2(a: UInt, b: Int): UInt = {
    if (b == 1) a
    else {
      require(isPow2(b))
      Cat(a, 0.U(CLog2Up(b).W))
    }
  }

  def manyOnes(n: Int): UInt = BigInt("1" * n, radix=2).U

  def manyOnesVec(n: Int): Vec[Bool] = VecInit(BigInt("1" * n, radix=2).U.asBools)


  /**
   * We might have a logical mask 0110 where each bit corresponds to a 16b payload, but most memory protocols
   * deal in 8b payloads, so we need to adjust the mask
   */
  def maskDemux(a: Vec[Bool], bitsPerBit: Int): UInt = {
    Cat(a.map(b => Mux(b, manyOnes(bitsPerBit), 0.U(bitsPerBit.W))))
  }

  def maskDemux(a: UInt, bitsPerBit: Int): UInt = {
    maskDemux(VecInit(a.asBools), bitsPerBit)
  }

}
