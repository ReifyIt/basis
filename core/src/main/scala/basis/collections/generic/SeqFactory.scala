//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import scala.reflect.macros._

trait SeqFactory[+CC[_]] extends CollectionFactory[CC] {
  def fill[A](count: Int)(elem: => A): CC[A]             = macro SeqFactoryMacros.fill[CC, A]
  def tabulate[A](count: Int)(f: Int => A): CC[A]        = macro SeqFactoryMacros.tabulate[CC, A]
  def iterate[A](start: A, count: Int)(f: A => A): CC[A] = macro SeqFactoryMacros.iterate[CC, A]

  implicit override def Factory: SeqFactory[CC] = this
}

private[generic] class SeqFactoryMacros(override val c: blackbox.Context { type PrefixType <: SeqFactory[CC] forSome { type CC[_] } }) extends FactoryMacros(c)
