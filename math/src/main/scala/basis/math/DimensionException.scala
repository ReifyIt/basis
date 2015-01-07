//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math

/** Indicates a dimensional mismatch.
  *
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Exceptions
  */
class DimensionException(message: String) extends RuntimeException(message) {
  def this() = this(null)
}
