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

import chisel3._

/** Represents a burst memory request. */
class BurstMemRequest[S <: Data, T <: Data](s: S, t: T) extends MemRequest(s, t) {
  /** Burst length */
  val burstLength = Output(UInt(8.W))

  override def cloneType: this.type = new BurstMemRequest(s, t).asInstanceOf[this.type]
}

object BurstMemRequest {
  /**
   * Constructs a burst memory request.
   *
   * @param rd          Read enable.
   * @param wr          Write enable.
   * @param burstLength The burst length.
   * @param addr        The address value.
   * @param data        The data value.
   */
  def apply[S <: Data, T <: Data](rd: Bool, wr: Bool, addr: S, data: T, burstLength: UInt): BurstMemRequest[S, T] = {
    val req = Wire(new BurstMemRequest(chiselTypeOf(addr), chiselTypeOf(data)))
    req.rd := rd
    req.wr := wr
    req.addr := addr
    req.data := data
    req.burstLength := burstLength
    req
  }
}
