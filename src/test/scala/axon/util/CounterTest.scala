/*
 *   __   __     __  __     __         __
 *  /\ "-.\ \   /\ \/\ \   /\ \       /\ \
 *  \ \ \-.  \  \ \ \_\ \  \ \ \____  \ \ \____
 *   \ \_\\"\_\  \ \_____\  \ \_____\  \ \_____\
 *    \/_/ \/_/   \/_____/   \/_____/   \/_____/
 *   ______     ______       __     ______     ______     ______
 *  /\  __ \   /\  == \     /\ \   /\  ___\   /\  ___\   /\__  _\
 *  \ \ \/\ \  \ \  __<    _\_\ \  \ \  __\   \ \ \____  \/_/\ \/
 *   \ \_____\  \ \_____\ /\_____\  \ \_____\  \ \_____\    \ \_\
 *    \/_____/   \/_____/ \/_____/   \/_____/   \/_____/     \/_/
 *
 * https://joshbassett.info
 * https://twitter.com/nullobject
 * https://github.com/nullobject
 *
 * Copyright (c) 2021 Josh Bassett
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

package axon.util

import chisel3._
import chiseltest._
import org.scalatest._

class CounterTest extends FlatSpec with ChiselScalatestTester with Matchers {
  it should "increment a static counter" in {
    test(new Module {
      val io = IO(new Bundle {
        val value = Output(UInt())
        val wrap = Output(Bool())
        val enable = Input(Bool())
        val reset = Input(Bool())
      })
      val (value, wrap) = Counter.static(3, enable = io.enable, reset = io.reset)
      io.value := value
      io.wrap := wrap
    }) { dut =>
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
      dut.clock.step()
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
      dut.io.enable.poke(true.B)
      dut.clock.step()
      dut.io.value.expect(1.U)
      dut.io.wrap.expect(false.B)
      dut.io.reset.poke(true.B)
      dut.clock.step()
      dut.io.reset.poke(false.B)
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
      dut.clock.step()
      dut.io.value.expect(1.U)
      dut.io.wrap.expect(false.B)
      dut.clock.step()
      dut.io.value.expect(2.U)
      dut.io.wrap.expect(true.B)
      dut.clock.step()
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
    }
  }

  it should "wrap a static counter with zero length" in {
    test(new Module {
      val io = IO(new Bundle {
        val value = Output(UInt())
        val wrap = Output(Bool())
      })
      val (value, wrap) = Counter.static(0)
      io.value := value
      io.wrap := wrap
    }) { dut =>
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(true.B)
      dut.clock.step()
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(true.B)
    }
  }

  it should "increment a dynamic counter" in {
    test(new Module {
      val io = IO(new Bundle {
        val value = Output(UInt())
        val wrap = Output(Bool())
        val enable = Input(Bool())
        val reset = Input(Bool())
      })
      val (value, wrap) = Counter.dynamic(3.U, enable = io.enable, reset = io.reset)
      io.value := value
      io.wrap := wrap
    }) { dut =>
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
      dut.clock.step()
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
      dut.io.enable.poke(true.B)
      dut.clock.step()
      dut.io.value.expect(1.U)
      dut.io.wrap.expect(false.B)
      dut.io.reset.poke(true.B)
      dut.clock.step()
      dut.io.reset.poke(false.B)
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
      dut.clock.step()
      dut.io.value.expect(1.U)
      dut.io.wrap.expect(false.B)
      dut.clock.step()
      dut.io.value.expect(2.U)
      dut.io.wrap.expect(true.B)
      dut.clock.step()
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(false.B)
    }
  }

  it should "wrap a dynamic counter with zero length" in {
    test(new Module {
      val io = IO(new Bundle {
        val value = Output(UInt())
        val wrap = Output(Bool())
      })
      val (value, wrap) = Counter.dynamic(0.U)
      io.value := value
      io.wrap := wrap
    }) { dut =>
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(true.B)
      dut.clock.step()
      dut.io.value.expect(0.U)
      dut.io.wrap.expect(true.B)
    }
  }

}
