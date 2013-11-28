//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

/** A control-flow break exception.
  *
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
class Break extends Throwable(null, null) {
  override def toString: String = "Break"
}
