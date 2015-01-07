//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._
import basis.text._

/** An iterable collection of elements.
  *
  * @define collection  container
  */
trait Container[+A] extends Any with Family[Container[_]] with Collection[A] {
  override def isEmpty: Boolean = iterator.isEmpty

  def iterator: Iterator[A]

  override def traverse(f: A => Unit): Unit = iterator.traverse(f)

  override def toString: String = {
    val s = String.Builder
    s.append(stringPrefix)
    s.append('(')
    val these = iterator
    if (!these.isEmpty) {
      s.show(these.head)
      these.step()
      while (!these.isEmpty) {
        s.append(", ")
        s.show(these.head)
        these.step()
      }
    }
    s.append(')')
    s.state
  }
}

object Container extends generic.CollectionFactory[Container] {
  override def empty[A]: Container[A] = immutable.List.empty[A]

  override def from[A](elems: Traverser[A]): Container[A] = {
    if (elems.isInstanceOf[Container[_]]) elems.asInstanceOf[Container[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Container[A]] =
    immutable.List.Builder[A]

  override def toString: String = "Container"
}
