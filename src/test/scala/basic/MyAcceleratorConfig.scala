package basic

import beethoven.Platforms.FPGA.Xilinx.F2.{AWSF2Platform, DMAHelperConfig, MemsetHelper, MemsetHelperConfig}
import beethoven._
import beethoven.common.{AccelCommand, Address, EmptyAccelResponse}
import chisel3._

class MyAcceleratorConfig extends AcceleratorConfig(List(
  AcceleratorSystemConfig(
    nCores = 1,
    name = "MyAcceleratorSystem",
    moduleConstructor = BlackboxBuilderCustom(new AccelCommand("my_accel") {
      val addend = UInt(32.W)
      val vec_addr = UInt(32.W)
      val n_eles = UInt(20.W)
    }, EmptyAccelResponse()),
//    moduleConstructor = ModuleBuilder(p => new MyAccelerator()(p)),
    memoryChannelConfig = List(
      ReadChannelConfig("vec_in", dataBytes = 4),
      WriteChannelConfig("vec_out", dataBytes = 4))
  ), new DMAHelperConfig, new MemsetHelperConfig(4)))

object MyAcceleratorKria extends BeethovenBuild(new MyAcceleratorConfig,
  buildMode = BuildMode.Simulation,
  platform = new AWSF2Platform)