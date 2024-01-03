package composer

import chipsalliance.rocketchip.config.Parameters
import composer.Platforms.PlatformNumSLRs

package object Systems {
  // this can be improved
  def core2slr(coreIdx: Int)(implicit p: Parameters): Int = coreIdx % p(PlatformNumSLRs)

}
