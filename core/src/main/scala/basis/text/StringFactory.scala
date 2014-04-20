//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis._

trait StringFactory[+S] {
  def empty: S = Builder.state

  def apply(chars: CharSequence): S = {
    val s = Builder
    s.append(chars)
    s.state
  }

  implicit def Builder: StringBuilder with State[S]
}
