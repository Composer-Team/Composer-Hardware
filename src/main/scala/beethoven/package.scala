import chipsalliance.rocketchip.config.Parameters
import beethoven.Platforms.PlatformKey

package object beethoven {
  def platform(implicit p: Parameters) = p(PlatformKey)
}
