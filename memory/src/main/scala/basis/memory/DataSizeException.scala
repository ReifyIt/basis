//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

/** Indicates an unsupported allocation size.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
class DataSizeException(message: String) extends RuntimeException(message) {
  def this() = this(null)
}
