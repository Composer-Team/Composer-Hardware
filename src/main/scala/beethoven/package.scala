
package object beethoven {
  import chipsalliance.rocketchip.config.Parameters
  import beethoven.Platforms.PlatformKey

  def platform(implicit p: Parameters) = p(PlatformKey)
}

