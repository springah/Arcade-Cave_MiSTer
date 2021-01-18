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

package axon.mem

import axon.util.Counter
import chisel3._
import chisel3.util._

/** An interface for reading and writing to DDR memory. */
class DDRIO protected(config: DDRConfig) extends AsyncReadWriteMemIO(config.addrWidth, config.dataWidth) with BurstIO

object DDRIO {
  def apply(config: DDRConfig) = new DDRIO(config: DDRConfig)
}

/**
 * Represents the DDR memory configuration.
 *
 * @param addrWidth The width of the address bus.
 * @param dataWidth The width of the data bus.
 * @param offset    The offset of the address.
 */
case class DDRConfig(addrWidth: Int = 32,
                     dataWidth: Int = 64,
                     offset: Int = 0)

/**
 * Handles reading/writing data to a DDR memory device.
 *
 * @param config The DDR configuration.
 */
class DDR(config: DDRConfig) extends Module {
  val io = IO(new Bundle {
    /** Memory port */
    val mem = Flipped(BurstReadWriteMemIO(config.addrWidth, config.dataWidth))
    /** DDR port */
    val ddr = DDRIO(config)
    /** Debug port */
    val debug = new Bundle {
      val burstCounter = Output(UInt())
    }
  })

  object State {
    val idle :: latch :: burstWait :: Nil = Enum(3)
  }

  // State register
  val nextState = Wire(UInt())
  val stateReg = RegNext(nextState, State.idle)

  // Request register
  val request = BurstMemRequest(io.mem.rd, io.mem.wr, io.mem.addr, io.mem.din, io.mem.burstLength)
  val requestReg = RegEnable(request, nextState === State.latch)

  // Control signals
  val read = stateReg === State.latch && requestReg.rd
  val write = (stateReg === State.latch && requestReg.wr) || (stateReg === State.burstWait && requestReg.wr)
  val effectiveRead = requestReg.rd && io.ddr.valid
  val effectiveWrite = requestReg.wr && !io.ddr.waitReq
  val burstCounterEnable =
    (stateReg === State.latch && effectiveWrite) ||
    (stateReg === State.burstWait && (effectiveRead || effectiveWrite))

  // Burst counter
  val (burstCounter, burstCounterWrap) = Counter.dynamic(requestReg.burstLength, burstCounterEnable)

  // Default to the previous state
  nextState := stateReg

  // FSM
  switch(stateReg) {
    // Wait for a request
    is(State.idle) {
      when(request.valid) { nextState := State.latch }
    }

    // Latch request
    is(State.latch) {
      when(burstCounterWrap) {
        nextState := State.idle
      }.elsewhen(requestReg.valid && !io.ddr.waitReq) {
        nextState := State.burstWait
      }
    }

    // Wait for burst
    is(State.burstWait) {
      when(burstCounterWrap) { nextState := State.idle }
    }
  }

  // Connect I/O ports
  io.mem <> io.ddr

  // Outputs
  io.mem.burstDone := burstCounterWrap
  io.ddr.rd := read
  io.ddr.wr := write
  io.ddr.addr := requestReg.addr +& config.offset.U
  io.ddr.burstLength := requestReg.burstLength
  io.debug.burstCounter := burstCounter

  printf(p"DDR(state: $stateReg, counter: $burstCounter ($burstCounterWrap)\n")
}
