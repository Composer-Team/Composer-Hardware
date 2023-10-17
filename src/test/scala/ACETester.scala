import chisel3.tester._
import chisel3._
import chiseltest.{ChiselScalatestTester, WriteFstAnnotation, WriteVcdAnnotation}
import composer.MemoryStreams.RAM.RegMem
import composer.Protocol.ACEZynqRegionManager
import freechips.rocketchip.subsystem.MasterPortParams
import org.scalatest.freespec.AnyFreeSpec

class ACETester extends AnyFreeSpec with ChiselScalatestTester {
  "it should enqueue" in test(new ACEZynqRegionManager(MasterPortParams(
    0, BigInt(1L) << 44, 16, 6), 32, { case (l, r, c) => val m = Module(new RegMem(r, c, 1, l)); m.io })).
    withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.out.arready.poke(false.B)
      c.out.awready.poke(false.B)
      c.out.acvalid.poke(false.B)
      c.out.cdready.poke(false.B)
      c.out.crready.poke(false.B)
      c.reset.poke(1.B)
      c.clock.step(30)
      c.reset.poke(0.B)
      c.clock.step(5)
      c.in_cmd.valid.poke(1.B)
      c.in_cmd.bits.poke(0x1ebae000L.U)
      c.clock.step()
      c.in_cmd.bits.poke(0.U)
      c.clock.step()
      // length is 1 << 8  - or 256B + 16B = 270B, or 17 cache lines, or two command beats: one at max length, one short
      c.in_cmd.bits.poke(0x110.U)
      c.clock.step()
      c.in_cmd.bits.poke(0.U)
      c.clock.step()
      // index is 1, so shift 2, op (bottom 2 bits) is 0 for add
      c.in_cmd.bits.poke((1 << 2).U)
      c.clock.step()
      c.in_cmd.valid.poke(0.B)
      c.clock.step(5)

      // now region should be valid in index 1
      // issue a invalidate cmd
      c.in_cmd.valid.poke(1.B)
      c.in_cmd.bits.poke(0.U)
      c.clock.step(4)
      // 1 is index (shift by 2), command is invalidate (2)
      c.in_cmd.bits.poke(((1 << 2) | 2).U)
      c.clock.step()
      c.in_cmd.valid.poke(0.B)
      c.clock.step(10)
      c.out.arready.poke(1.B)
      // expect a transaction to have started
      c.out.arvalid.expect(1.B)
      c.out.araddr.expect(0x1ebae000L.U)
      // max transaction length is 16
      c.out.arlen.expect(15.U)
      c.out.arburst.expect(0.U)
      c.out.arlock.expect(0.B)
      c.out.arcache.expect(0xB.U)
      c.out.arprot.expect(0.U)
      c.out.arregion.expect(0.U)
      c.out.arqos.expect(0.U)
      c.out.arid.expect(0.U)
      c.clock.step()
      c.out.arready.poke(0.B)
      c.clock.step(15)
      c.out.rvalid.poke(1.B)
      c.out.rlast.poke(1.B)
      c.clock.step()
      c.out.rack.expect(1.B)
      c.clock.step()
      c.out.arready.poke(1.B)
      c.out.arlen.expect(0.U)
      c.out.arburst.expect(0.U)
      c.out.arlock.expect(0.B)
      c.out.arcache.expect(0xB.U)
      c.out.arprot.expect(0.U)
      c.out.arregion.expect(0.U)
      c.out.arqos.expect(0.U)
      c.out.arid.expect(0.U)
      c.out.araddr.expect(0x1ebae100L.U)


    }
}
