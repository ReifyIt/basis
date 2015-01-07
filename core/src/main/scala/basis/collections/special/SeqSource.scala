//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

import scala.reflect.macros._

trait SeqSource[+CC, -A] extends CollectionSource[CC, A] {
  def fill(count: Int)(elem: => A): CC             = macro SeqSourceMacros.fill[CC, A]
  def tabulate(count: Int)(f: Int => A): CC        = macro SeqSourceMacros.tabulate[CC, A]
  def iterate(start: A, count: Int)(f: A => A): CC = macro SeqSourceMacros.iterate[CC, A]
}

private[special] class SeqSourceMacros(override val c: blackbox.Context { type PrefixType <: SeqSource[_, _] }) extends SourceMacros(c)