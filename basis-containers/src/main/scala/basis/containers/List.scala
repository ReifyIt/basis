/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

/** An immutable singly-linked list.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Containers
  * 
  * @groupprio  Measuring     1
  * @groupprio  Decomposing   2
  * @groupprio  Composing     3
  * @groupprio  Slicing       4
  * @groupprio  Traversing    5
  * @groupprio  Exporting     6
  * @groupprio  Classifying   7
  * 
  * @define collection  list
  */
sealed abstract class List[+A]
  extends Equals
    with Immutable
    with Family[List[_]]
    with ListLike[A]
    with Stack[A] {
  
  override def tail: List[A]
  
  /** Returns the number of elements in this $collection.
    * @group Measuring */
  def length: Int = {
    var n = 0
    var xs = this
    while (!xs.isEmpty) {
      n += 1
      xs = xs.tail
    }
    n
  }
  
  /** Returns the `lower` tail of this $collection.
    * @group Slicing */
  def drop(lower: Int): List[A] = {
    var i = 0
    var xs = this
    while (i < lower && !xs.isEmpty) {
      i += 1
      xs = xs.tail
    }
    xs
  }
  
  /** Returns the `upper` heads of this $collection.
    * @group Slicing */
  def take(upper: Int): List[A] = {
    var i = 0
    val b = new ListBuilder[A]
    var xs = this
    while (i < upper && !xs.isEmpty) {
      i += 1
      b += xs.head
      xs = xs.tail
    }
    b.state
  }
  
  /** Returns the [`lower`, `upper`) bounded elements of this $collection.
    * @group Slicing */
  def slice(lower: Int, upper: Int): List[A] =
    if (lower < upper) drop(lower).take(upper) else Nil
  
  /** Returns this $collection with the given element prepended.
    * @group Composing */
  def :: [B >: A](elem: B): List[B] = new RefList(elem, this)
  
  /** Returns the reverse of this $collection.
    * @group Composing */
  def reverse: List[A] = {
    var sx = Nil: List[A]
    var xs = this
    while (!xs.isEmpty) {
      sx = new RefList(xs.head, sx)
      xs = xs.tail
    }
    sx
  }
  
  override def traverse(f: A => Unit) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
  
  override def iterator: Iterator[A] = new RefListIterator(this)
  
  override def toList: this.type = this
  
  protected override def stringPrefix: String = "List"
}

/** A factory for [[List singly-linked lists]].
  * @group Containers */
object List extends SeqFactory[List, TypeHint] {
  implicit override def Builder[A : TypeHint]
    : Builder[A] { type Scope = List[_]; type State = List[A] } =
    new ListBuilder[A]
  
  override def empty[A : TypeHint]: List[A] = Nil
  
  override def coerce[A : TypeHint](elems: Enumerator[A]): List[A] =
    if (elems.isInstanceOf[List[_]]) elems.asInstanceOf[List[A]]
    else if (elems.isInstanceOf[ListLike[_]]) elems.asInstanceOf[ListLike[A]].toList
    else super.coerce(elems)
  
  override def toString: String = "List"
}

/** A [[List]] cons cell.
  * @group Containers */
sealed abstract class ::[A] extends List[A] {
  final override def isEmpty: Boolean = false
  
  private[containers] def tail_=(tail: List[A]): Unit
}

private[containers] final class IntList(car: Int, cdr: List[Int]) extends ::[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override val head: Int = car
  
  override var tail: List[Int] = cdr
  
  override def :: [B >: Int](elem: B): List[B] = {
    if (elem.isInstanceOf[Int]) new IntList(elem.asInstanceOf[Int], this)
    else new RefList(elem, this)
  }
  
  override def reverse: List[Int] = {
    var sx = Nil: List[Int]
    var xs = this: List[Int]
    while (!xs.isEmpty) {
      sx = new IntList(xs.asInstanceOf[IntList].head, sx)
      xs = xs.tail
    }
    sx
  }
  
  override def traverse(f: Int => Unit) {
    var xs = this: List[Int]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
  
  override def iterator: Iterator[Int] = new IntListIterator(this)
}

private[containers] final class LongList(car: Long, cdr: List[Long]) extends ::[Long] with Reified {
  protected override def T: TypeHint[Long] = TypeHint.Long
  
  override val head: Long = car
  
  override var tail: List[Long] = cdr
  
  override def :: [B >: Long](elem: B): List[B] = {
    if (elem.isInstanceOf[Long]) new LongList(elem.asInstanceOf[Long], this)
    else new RefList(elem, this)
  }
  
  override def reverse: List[Long] = {
    var sx = Nil: List[Long]
    var xs = this: List[Long]
    while (!xs.isEmpty) {
      sx = new LongList(xs.asInstanceOf[LongList].head, sx)
      xs = xs.tail
    }
    sx
  }
  
  override def traverse(f: Long => Unit) {
    var xs = this: List[Long]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
  
  override def iterator: Iterator[Long] = new LongListIterator(this)
}

private[containers] final class FloatList(car: Float, cdr: List[Float]) extends ::[Float] with Reified {
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override val head: Float = car
  
  override var tail: List[Float] = cdr
  
  override def :: [B >: Float](elem: B): List[B] = {
    if (elem.isInstanceOf[Float]) new FloatList(elem.asInstanceOf[Float], this)
    else new RefList(elem, this)
  }
  
  override def reverse: List[Float] = {
    var sx = Nil: List[Float]
    var xs = this: List[Float]
    while (!xs.isEmpty) {
      sx = new FloatList(xs.asInstanceOf[FloatList].head, sx)
      xs = xs.tail
    }
    sx
  }
  
  override def traverse(f: Float => Unit) {
    var xs = this: List[Float]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
  
  override def iterator: Iterator[Float] = new FloatListIterator(this)
}

private[containers] final class DoubleList(car: Double, cdr: List[Double]) extends ::[Double] with Reified {
  protected override def T: TypeHint[Double] = TypeHint.Double
  
  override val head: Double = car
  
  override var tail: List[Double] = cdr
  
  override def :: [B >: Double](elem: B): List[B] = {
    if (elem.isInstanceOf[Double]) new DoubleList(elem.asInstanceOf[Double], this)
    else new RefList(elem, this)
  }
  
  override def reverse: List[Double] = {
    var sx = Nil: List[Double]
    var xs = this: List[Double]
    while (!xs.isEmpty) {
      sx = new DoubleList(xs.asInstanceOf[DoubleList].head, sx)
      xs = xs.tail
    }
    sx
  }
  
  override def traverse(f: Double => Unit) {
    var xs = this: List[Double]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
  
  override def iterator: Iterator[Double] = new DoubleListIterator(this)
}

private[containers] final class RefList[A](car: A, cdr: List[A]) extends ::[A] {
  override val head: A = car
  
  override var tail: List[A] = cdr
}

/** An extractor for [[List]] cons cells.
  * @group Containers */
object :: {
  def apply[A](x: A, xs: List[A]): ::[A] = {
    if (x.isInstanceOf[Int] && (xs.isInstanceOf[IntList] || xs.isInstanceOf[Nil.type]))
      new IntList(x.asInstanceOf[Int], xs.asInstanceOf[List[Int]]).asInstanceOf[::[A]]
    else if (x.isInstanceOf[Long] && (xs.isInstanceOf[LongList] || xs.isInstanceOf[Nil.type]))
      new LongList(x.asInstanceOf[Long], xs.asInstanceOf[List[Long]]).asInstanceOf[::[A]]
    else if (x.isInstanceOf[Float] && (xs.isInstanceOf[FloatList] || xs.isInstanceOf[Nil.type]))
      new FloatList(x.asInstanceOf[Float], xs.asInstanceOf[List[Float]]).asInstanceOf[::[A]]
    else if (x.isInstanceOf[Double] && (xs.isInstanceOf[DoubleList] || xs.isInstanceOf[Nil.type]))
      new DoubleList(x.asInstanceOf[Double], xs.asInstanceOf[List[Double]]).asInstanceOf[::[A]]
    else new RefList(x, xs)
  }
  
  def unapply[A](list: ::[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}

/** The empty [[List]].
  * @group Containers */
object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing = throw new NoSuchElementException("Head of empty list.")
  
  override def tail: List[Nothing] = throw new UnsupportedOperationException("Tail of empty list.")
  
  override def :: [B](elem: B): List[B] = {
    if (elem.isInstanceOf[Int]) new IntList(elem.asInstanceOf[Int], this).asInstanceOf[List[B]]
    else if (elem.isInstanceOf[Long]) new LongList(elem.asInstanceOf[Long], this).asInstanceOf[List[B]]
    else if (elem.isInstanceOf[Float]) new FloatList(elem.asInstanceOf[Float], this).asInstanceOf[List[B]]
    else if (elem.isInstanceOf[Double]) new DoubleList(elem.asInstanceOf[Double], this).asInstanceOf[List[B]]
    else new RefList(elem, this)
  }
  
  override def traverse(f: Nothing => Unit): Unit = ()
}

private[containers] final class IntListIterator(private[this] var xs: List[Int]) extends Iterator[Int] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: Int = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.asInstanceOf[IntList].head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[Int] = new IntListIterator(xs)
}

private[containers] final class LongListIterator(private[this] var xs: List[Long]) extends Iterator[Long] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: Long = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.asInstanceOf[LongList].head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[Long] = new LongListIterator(xs)
}

private[containers] final class FloatListIterator(private[this] var xs: List[Float]) extends Iterator[Float] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: Float = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.asInstanceOf[FloatList].head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[Float] = new FloatListIterator(xs)
}

private[containers] final class DoubleListIterator(private[this] var xs: List[Double]) extends Iterator[Double] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: Double = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.asInstanceOf[DoubleList].head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[Double] = new DoubleListIterator(xs)
}

private[containers] final class RefListIterator[+A](private[this] var xs: List[A]) extends Iterator[A] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: A = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[A] = new RefListIterator(xs)
}

private[containers] final class ListBuilder[A] extends ListBuffer[A] {
  override type Scope = List[_]
  override type State = List[A]
  override def state: List[A] = toList
  override def toString: String = "ListBuilder"
}
