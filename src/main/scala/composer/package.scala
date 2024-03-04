import chipsalliance.rocketchip.config.Parameters
import composer.Platforms.PlatformKey

package object composer {
  def platform(implicit p: Parameters) = p(PlatformKey)
}
