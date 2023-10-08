package composer.common

import chisel3._

//object ClockCross {
//  def apply[T <: Data](fromRate: Int, toRate: Int, d: T, toClock: Clock): T = {
//    if (fromRate < toRate) {
//      // going from slow to fast
//      val mult = (toRate.toFloat / fromRate).ceil.toInt
//      val outWire = Wire(d)
//      withClock(toClock) {
//        outWire := ShiftReg(d, mult)
//      }
//      outWire
//    } else if (fromRate > toRate) {
//      // going from fast to slow, stretch
//      val mult = (fromRate.toFloat / toRate).ceil.toInt
//
//    }
//  }
//}
