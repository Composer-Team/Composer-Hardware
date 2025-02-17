package basic

import beethoven.Platforms.FPGA.Xilinx.F2.AWSF2Platform
import beethoven._

class MyAcceleratorConfig extends AcceleratorConfig(
  AcceleratorSystemConfig(
    nCores = 1,
    name = "MyAccelerator",
    moduleConstructor = ModuleBuilder(p => new MyAccelerator()(p)),
    memoryChannelConfig = List(
      ReadChannelConfig("vec_in", dataBytes = 4),
      WriteChannelConfig("vec_out", dataBytes = 4))
  ))
object MyAcceleratorKria extends BeethovenBuild(new MyAcceleratorConfig,
  buildMode = BuildMode.Synthesis,
  platform = new AWSF2Platform)