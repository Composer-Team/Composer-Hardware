package beethoven.common

import beethoven.common.Address.addrBits
import beethoven.platform
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.log2Up

import scala.language.implicitConversions

case class Address()(implicit p: Parameters) extends Bundle {

  val address = UInt(addrBits().W)

  implicit def toUInt: UInt = address

  // override + operator
  def +(that: UInt): Address = {
    val a = Wire(new Address())
    a.address := this.address + that
    a
  }
}

object AddressInit {
  def apply(in: UInt)(implicit p: Parameters): Address = {
    val a = Wire(Address())
    a.address := in
    a
  }
}

object Address {
  def addrBits()(implicit p: Parameters): Int = log2Up(platform.extMem.master.size)
  def apply(in: UInt)(implicit p: Parameters): Address = {
    val a = Wire(Address())
    a.address := in
    a
  }
  implicit def addrToUInt(address: Address): UInt = address.address
}