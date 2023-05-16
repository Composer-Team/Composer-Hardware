package composer.common

import chisel3._

class BUFG extends BlackBox {
  val io = IO(new Bundle() {
    val I = Input(Bool())
    val O = Output(Bool())
  })
}
