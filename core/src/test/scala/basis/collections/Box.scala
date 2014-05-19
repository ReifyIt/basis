//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

private[collections] final class Box(val value: Int) {
  override def equals(other: Any): Boolean = other match {
    case that: Box => value == that.value
    case _ => false
  }

  override def hashCode: Int = value.##

  override def toString: String = value.toString
}
