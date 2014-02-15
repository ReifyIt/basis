//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

trait SeqFactory[+CC[_]] extends CollectionFactory[CC] {
  def fill[A](count: Int)(elem: => A): CC[A]             = macro SeqFactory.fill[CC, A]
  def tabulate[A](count: Int)(f: Int => A): CC[A]        = macro SeqFactory.tabulate[CC, A]
  def iterate[A](start: A, count: Int)(f: A => A): CC[A] = macro SeqFactory.iterate[CC, A]

  implicit override def Factory: SeqFactory[CC] = this
}

private[generic] object SeqFactory extends FactoryMacros[SeqFactory]
