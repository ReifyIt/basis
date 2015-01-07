//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A once traversable collection of elements.
  *
  * @define collection  traverser
  */
trait Traverser[+A] extends Any with Family[Traverser[_]] {
  def traverse(f: A => Unit): Unit
}

object Traverser extends generic.CollectionFactory[Traverser] {
  override def empty[A]: Traverser[A] = immutable.List.empty[A]

  override def from[A](elems: Traverser[A]): Traverser[A] = elems

  implicit override def Builder[A]: Builder[A] with State[Traverser[A]] = immutable.List.Builder[A]

  override def toString: String = "Traverser"
}
