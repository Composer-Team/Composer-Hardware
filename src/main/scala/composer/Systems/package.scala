package composer

import chipsalliance.rocketchip.config.Parameters
import composer.Platforms.{MultiDiePlatform, Platform, PlatformKey}

package object Systems {
  // this can be improved
  def core2slr(coreIdx: Int)(implicit p: Parameters): Int = coreIdx % (
    p(PlatformKey) match {
      case pwmd: Platform with MultiDiePlatform => pwmd.platformDies.length
      case _ => 1
    })

}
