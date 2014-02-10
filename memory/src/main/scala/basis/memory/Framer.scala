//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._

/** A growable pointer.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
abstract class Framer extends Writer with State[Loader] {
  def size: Long

  def expect(count: Long): this.type

  def state: State

  def clear(): Unit
}
