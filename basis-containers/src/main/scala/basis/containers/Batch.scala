/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

import scala.Predef.<:<

/** An index-mapped finger tree.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Containers
  * 
  * @groupprio  Measuring     1
  * @groupprio  Indexing      2
  * @groupprio  Decomposing   3
  * @groupprio  Combining     4
  * @groupprio  Slicing       5
  * @groupprio  Traversing    6
  * @groupprio  Classifying   7
  * 
  * @define collection  batch
  */
abstract class Batch[+A] private[containers]
  extends Equals
    with Immutable
    with Family[Batch[_]]
    with Index[A]
    with Deque[A] {
  
  override def body: Batch[A]
  
  override def tail: Batch[A]
  
  /** Returns a copy of this $collection with the given element at the given index.
    * @group Indexing */
  def update[B >: A](index: Int, elem: B): Batch[B]
  
  /** Returns all but the first `lower` elements of this $collection.
    * @group Slicing */
  def drop(lower: Int): Batch[A]
  
  /** Returns the first `upper` elements of this $collection.
    * @group Slicing */
  def take(upper: Int): Batch[A]
  
  /** Returns a copy of this $collection with the given element appended.
    * @group Combining */
  def append[B >: A](elem: B): Batch[B]
  
  /** Returns a copy of this $collection with the given element prepended.
    * @group Combining */
  def prepend[B >: A](elem: B): Batch[B]
  
  override def :+ [B >: A](elem: B): Batch[B] = append(elem)
  
  override def +: [B >: A](elem: B): Batch[B] = prepend(elem)
  
  override def :: [B >: A](elem: B): Batch[B] = prepend(elem)
  
  override def traverse(f: A => Unit): Unit
  
  /** Applies a side-effecting function to each nested element of this $collection.
    * @group Traversing */
  def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]) {
    var i = 0
    val n = length
    while (i < n) {
      this(i) traverse f
      i += 1
    }
  }
  
  protected override def stringPrefix: String = "Batch"
}

/** A factory for [[Batch batches]].
  * @group Containers */
object Batch extends SeqFactory[Batch, TypeHint] {
  implicit override def Builder[A : TypeHint]
    : Builder[A] { type Scope = Batch[_]; type State = Batch[A] } =
    new BatchBuilder[A]
  
  override def empty[A : TypeHint]: Batch[A] = Empty
  
  override def coerce[A : TypeHint](elems: Enumerator[A]): Batch[A] =
    if (elems.isInstanceOf[Batch[_]]) elems.asInstanceOf[Batch[A]]
    else super.coerce(elems)
  
  override def toString: String = "Batch"
  
  private[containers] object Empty extends Batch[Nothing] {
    override def isEmpty: Boolean = true
    
    override def length: Int = 0
    
    override def apply(index: Int): Nothing = throw new IndexOutOfBoundsException(index.toString)
    
    override def update[B](index: Int, elem: B): Batch[B] = throw new IndexOutOfBoundsException(index.toString)
    
    override def head: Nothing = throw new NoSuchElementException("head of empty batch")
    
    override def tail: Batch[Nothing] = throw new UnsupportedOperationException("tail of empty batch")
    
    override def body: Batch[Nothing] = throw new UnsupportedOperationException("init of empty batch")
    
    override def foot: Nothing = throw new NoSuchElementException("last of empty batch")
    
    override def drop(lower: Int): Batch[Nothing] = this
    
    override def take(upper: Int): Batch[Nothing] = this
    
    override def append[B](elem: B): Batch[B] = {
      if (elem.isInstanceOf[Int]) new IntBatch1(elem.asInstanceOf[Int]).asInstanceOf[Batch[B]]
      else if (elem.isInstanceOf[Long]) new LongBatch1(elem.asInstanceOf[Long]).asInstanceOf[Batch[B]]
      else if (elem.isInstanceOf[Float]) new FloatBatch1(elem.asInstanceOf[Float]).asInstanceOf[Batch[B]]
      else if (elem.isInstanceOf[Double]) new DoubleBatch1(elem.asInstanceOf[Double]).asInstanceOf[Batch[B]]
      else new RefBatch1(elem)
    }
    
    override def prepend[B](elem: B): Batch[B] = {
      if (elem.isInstanceOf[Int]) new IntBatch1(elem.asInstanceOf[Int]).asInstanceOf[Batch[B]]
      else if (elem.isInstanceOf[Long]) new LongBatch1(elem.asInstanceOf[Long]).asInstanceOf[Batch[B]]
      else if (elem.isInstanceOf[Float]) new FloatBatch1(elem.asInstanceOf[Float]).asInstanceOf[Batch[B]]
      else if (elem.isInstanceOf[Double]) new DoubleBatch1(elem.asInstanceOf[Double]).asInstanceOf[Batch[B]]
      else new RefBatch1(elem)
    }
    
    override def traverse(f: Nothing => Unit): Unit = ()
    
    override def flatTraverse[B](f: B => Unit)(implicit isNested: Nothing <:< Batch[B]): Unit = ()
  }
}

private[containers] final class BatchBuilder[A] extends Builder[A] {
  override type Scope = Batch[_]
  
  override type State = Batch[A]
  
  private[this] var these: Batch[A] = Batch.Empty
  
  override def append(elem: A): Unit = these = these :+ elem
  
  override def appendAll(elems: Enumerator[A]) {
    if (these.isEmpty && elems.isInstanceOf[Batch[_]])
      these = elems.asInstanceOf[Batch[A]]
    else super.appendAll(elems)
  }
  
  override def expect(count: Int): this.type = this
  
  override def state: Batch[A] = these
  
  override def clear(): Unit = these = Batch.Empty
  
  override def toString: String = "BatchBuilder"
}
