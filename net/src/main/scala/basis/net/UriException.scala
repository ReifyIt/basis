//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis.collections._
import basis.text._

class UriException(message: String, val input: Iterator[Int]) extends RuntimeException(message) {
  def this(message: String) = this(message, Iterator.empty)

  override def toString: String = (String.Builder~"UriException"~'('~>message~", "~>input~')').state
}
