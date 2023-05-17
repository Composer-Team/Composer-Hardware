package composer.common

import chisel3._
import chisel3.util._

import scala.collection.SeqMap

trait hasAccessibleUserSubRegions {
  val reservedNames: Seq[String]

  val elements: SeqMap[String, Data]

  private[composer] def realDatas: SeqMap[String, Data] = elements.filter(ele => !reservedNames.contains(ele._1))

  private[composer] def realElements: Seq[(String, Data)] = realDatas.toSeq.map(a => (a._1, a._2)).sortBy(_._1)

  private[composer] def fieldSubranges: Seq[(String, (Int, Int))] = {
    val eles = realElements
    val scan = eles.map(_._2.getWidth).scan(0)(_ + _).tail
    eles zip scan map { case (e, top) => (e._1, (top - 1, top - e._2.getWidth)) }
  }
}

object hasAccessibleUserSubRegions {
  def apply[T <: hasAccessibleUserSubRegions with Bundle](in: UInt, gen: T): T = {
    val a = Wire(gen)
    a.fieldSubranges.foreach{ case (ele_name, (high, low)) =>
      a.elements(ele_name) := in(high, low)
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