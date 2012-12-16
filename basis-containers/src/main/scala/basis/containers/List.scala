/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** A singly linked list.
  * 
  * @groupprio  Examining     -8
  * @groupprio  Mutating      -7
  * @groupprio  Inserting     -6
  * @groupprio  Removing      -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Converting    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  list
  */
sealed abstract class List[+A]
  extends Equals
    with Family[List[A]]
    with Stack[A]
    with ListLike[A] {
  
  override def tail: List[A]
  
  @tailrec final def drop(lower: Int): List[A] =
    if (lower <= 0 || isEmpty) this else tail.drop(lower - 1)
  
  final def take(upper: Int): List[A] = {
    var xs = this
    val b = new ListBuilder[A]
    var i = 0
    while (i < upper && !xs.isEmpty) {
      i += 1
      b += xs.head
      xs = xs.tail
    }
    b.state
  }
  
  final def slice(lower: Int, upper: Int): List[A] =
    if (lower >= upper) Nil else drop(lower).take(upper)
  
  final override def length: Int = length(0)
  @tailrec private final def length(count: Int): Int =
    if (isEmpty) count else tail.length(count + 1)
  
  final def reverse: List[A] = reverse(Nil, this)
  @tailrec private[this] def reverse(sx: List[A], xs: List[A]): List[A] =
    if (xs.isEmpty) sx else reverse(xs.head :: sx, xs.tail)
  
  final def :: [B >: A](elem: B): List[B] = new ::[B](elem, this)
  
  final override def toList: this.type = this
  
  final override def iterator: Iterator[A] = new ListIterator(this)
  
  @tailrec protected final override def foreach[U](f: A => U) =
    if (!isEmpty) { f(head); tail.foreach[U](f) }
  
  protected override def stringPrefix: String = "List"
}

final class ::[A](private[this] var x: A, private[this] var xs: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
  
  override def head: A = x
  
  private[containers] def head_=(head: A): Unit = x = head
  
  override def tail: List[A] = xs
  
  private[containers] def tail_=(tail: List[A]): Unit = xs = tail
}

object :: {
  def unapply[A](list: ::[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing =
    throw new NoSuchElementException("Head of empty list,")
  
  override def tail: List[Nothing] =
    throw new UnsupportedOperationException("Tail of empty list.")
}

object List extends SeqFactory[List] {
  // FIXME: don't hurt the compiler!
  //override def apply[A](xs: A*): List[A] = macro ListMacros.apply[A]
  
  implicit override def Builder[A : ClassTag]
    : Builder[Any, A] { type State = List[A] } =
    new ListBuilder
  
  override def toString: String = "List"
}

private[containers] object ListMacros {
  import scala.reflect.macros.Context
  
  def apply[A : c.WeakTypeTag](c: Context)(xs: c.Expr[A]*): c.Expr[List[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val ListType =
      appliedType(
        mirror.staticClass("basis.containers.List").toType,
        weakTypeOf[A] :: scala.collection.immutable.Nil)
    var b: Tree = Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "containers"), "Nil")
    val iter = xs.reverseIterator
    while (iter.hasNext) b = Apply(Select(b, "$colon$colon"), iter.next().tree :: scala.collection.immutable.Nil)
    Expr(b)(WeakTypeTag(ListType))
  }
}

private[containers] final class ListIterator[+A](private[this] var xs: List[A]) extends Iterator[A] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: A = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    else xs.head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    else xs = xs.tail
  }
  
  override def dup: Iterator[A] = new ListIterator(xs)
}

private[containers] final class ListBuilder[A] extends ListBuffer[A] with Builder[Any, A] {
  override type State = List[A]
  
  override def expect(count: Int): this.type = this
  
  override def state: List[A] = toList
  
  protected override def stringPrefix: String = "List.Builder"
}
