package beethoven

case class AcceleratorConfig(systems: List[AcceleratorSystemConfig]) {
  def this(config: AcceleratorSystemConfig) = {
    this(List(config))
  }
  def ++(other: AcceleratorConfig): AcceleratorConfig = {
    AcceleratorConfig(other.systems ++ this.systems)
  }
}
