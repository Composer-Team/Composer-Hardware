package beethoven

import chipsalliance.rocketchip.config.Parameters
import beethoven.Platforms.{MultiDiePlatform, Platform, PlatformKey}

import scala.annotation.tailrec

package object Systems {
  def core2slr(coreIdx: Int)(implicit p: Parameters): Int = {
    p(PlatformKey) match {
      case pwmd: Platform with MultiDiePlatform =>
        @tailrec
        def recursivelyDecide(allocations: Seq[Double], nCores: Int): Seq[Double] = {
          if (nCores == 0) allocations
          else {
            val min = allocations.min
            val idx = allocations.indexOf(min)
            val affinity = 1.0 / pwmd.placementAffinity(idx)
            val newAllocations = allocations.updated(idx, min + affinity)
            recursivelyDecide(newAllocations, nCores - 1)
          }
        }
        val allocations = recursivelyDecide(Seq.fill(pwmd.placementAffinity.length)(0.0), coreIdx)
        allocations.indexOf(allocations.min)
      case _ => 0
    }
  }
}
