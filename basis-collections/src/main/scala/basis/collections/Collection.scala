//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis.util._

trait Collection[+A] extends Any with Family[Collection[_]] with Traverser[A] {
  def isEmpty: Boolean = {
    try { traverse(Collection.IsEmpty); true }
    catch { case signal: Break if signal eq begin.signal => false }
  }

  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix)
    s.append('(')
    traverse(new Collection.AddString(s))
    s.append(')')
    s.toString
  }

  protected def stringPrefix: String = getClass.getSimpleName
}

object Collection extends generic.CollectionFactory[Collection] {
  override def empty[A]: Collection[A] = immutable.List.empty[A]

  override def from[A](elems: Traverser[A]): Collection[A] = {
    if (elems.isInstanceOf[Collection[_]]) elems.asInstanceOf[Collection[A]]
    else super.from(elems)
  }

  implicit override def Builder[A](): Builder[A] with State[Collection[A]] =
    immutable.List.Builder[A]()

  override def toString: String = "Collection"

  private object IsEmpty extends scala.runtime.AbstractFunction1[Any, Unit] {
    override def apply(x: Any): Unit = begin.break()
  }

  private final class AddString(s: java.lang.StringBuilder) extends scala.runtime.AbstractFunction1[Any, Unit] {
    private[this] var e: Boolean = true
    override def apply(x: Any): Unit = (if (e) { e = false; s } else s.append(", ")).show(x)
  }
}
