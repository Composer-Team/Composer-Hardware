package beethoven

import chipsalliance.rocketchip.config.Config

case class AcceleratorConfig(configs: List[AcceleratorSystemConfig]) {
  private[beethoven] def toConfig: Config = {
    new Config((site, _, up) => {
      case AcceleratorSystems => up(AcceleratorSystems, site) ++ configs
    })
  }

  def ++(other: AcceleratorConfig): AcceleratorConfig = {
    AcceleratorConfig(configs ++ other.configs)
  }

  def this(config: AcceleratorSystemConfig) = {
    this(List(config))
  }

  def this(other: AcceleratorConfig) = {
    this(other.configs)
  }
}