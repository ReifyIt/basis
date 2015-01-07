//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import scala.annotation.unchecked._

trait ByteOrder[+E <: Endianness] extends Any {
  type Endian = E @uncheckedVariance

  /** Returns the internal byte order.
    * @group General */
  def endian: Endian
}
