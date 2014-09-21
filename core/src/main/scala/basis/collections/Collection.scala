//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._
import basis.text._
import basis.util._

/** A repeatedly traversable collection of elements.
  *
  * @define collection  collection
  */
trait Collection[+A] extends Any with Family[Collection[_]] with Traverser[A] {
  def isEmpty: Boolean = {
    try { traverse(Collection.IsEmpty); true }
    catch { case signal: Break if signal eq begin.signal => false }
  }

  override def toString: String = {
    val s = StringBuilder
    s.append(stringPrefix)
    s.append('(')
    traverse(new Collection.AddString(s))
    s.append(')')
    s.state
  }

  protected def stringPrefix: String = getClass.getSimpleName
}

object Collection extends generic.CollectionFactory[Collection] {
  import scala.runtime._

  override def empty[A]: Collection[A] = immutable.List.empty[A]

  override def from[A](elems: Traverser[A]): Collection[A] = {
    if (elems.isInstanceOf[Collection[_]]) elems.asInstanceOf[Collection[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Collection[A]] =
    immutable.List.Builder[A]

  override def toString: String = "Collection"

  private object IsEmpty extends AbstractFunction1[Any, Unit] {
    override def apply(x: Any): Unit = begin.break()
  }

  private final class AddString(s: StringBuilder) extends AbstractFunction1[Any, Unit] {
    private[this] var e: Boolean = true
    override def apply(x: Any): Unit = {
      if (e) e = false else s.append(", ")
      s.show(x)
    }
  }
}
