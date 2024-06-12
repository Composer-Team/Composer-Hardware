package beethoven.Generation.Tune

import chisel3._
import chisel3.experimental._
import beethoven.Generation.Stage.ExportCSymbolAnnotation
import firrtl.annotations.Annotation

abstract class DesignObjective {
  val name: Option[String]
  def getObjective: String = {
    name match {
      case Some(a) => f"\"$name\""
      case None => "null"
    }
  }
}

class MaximizeObjective extends DesignObjective {
  val name = Some("max")
}

class MinimizeObjective extends DesignObjective {
  val name = Some("min")
}

class NoObjective extends DesignObjective {
  val name = None
}

class PerfCounter(val name: String)

object DesignObjective {
  def annotateDesignObjective[T <: Data](dat: T, dobj: DesignObjective): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = {
        ExportCSymbolAnnotation(dat.toNamed, dobj)
      }
    })
  }
}

object PerfCounter {
  def apply[T <: Data](dat: T, designObjective: DesignObjective = new NoObjective): T = {
    DesignObjective.annotateDesignObjective(dat, designObjective)
    dontTouch(dat)
  }
}

//object EventPerformanceCounter {
//  def apply(event: Bool, designObjective: DesignObjective)(implicit valName: ValName, compileOptions: CompileOptions): Bool = {
//    val eventCounter = RegInit(UInt(64.W))
//    eventCounter.suggestName(valName.name + "_perfCounter")
//    eventCounter := eventCounter + 1.U
//    DesignObjective.annotateDesignObjective(eventCounter, designObjective)
//    dontTouch(eventCounter)
//    dontTouch(event)
//    event
//  }
//
//  def apply(events: Seq[Bool], designObjective: DesignObjective)(implicit valName: ValName, compileOptions: CompileOptions): Unit = {
//    val eventCounter = Reg(UInt(64.W))
//    eventCounter.suggestName(valName.name + "_perfCounter")
//    eventCounter := eventCounter + util.PopCount(events)
//    dontTouch(eventCounter)
//    DesignObjective.annotateDesignObjective(eventCounter, designObjective)
//  }
//}
