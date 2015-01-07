//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis._

final class String1(codeUnits: Array[Byte]) extends Equals with Family[String1] with UTF8 {
  override def size: Int = codeUnits.length

  override def get(index: Int): Int = codeUnits(index) & 0xFF
}

object String1 extends StringFactory[String1] {
  override val empty: String1 = new String1(new Array[Byte](0))

  implicit override def Builder: StringBuilder with State[String1] = new String1Builder

  override def toString: String = "String1"
}

private[text] final class String1Builder extends UTF8Encoder with State[String1] {
  override def state: String1 = new String1(alias)

  override def toString: String = "String1"+"."+"Builder"
}
