/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

/** A finger tree.
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
  * @groupprio  Quantifying   1
  * @groupprio  Decomposing   2
  * @groupprio  Inserting     3
  * @groupprio  Slicing       4
  * @groupprio  Iterating     5
  * @groupprio  Traversing    6
  * @groupprio  Classifying   7
  * 
  * @define collection  batch
  */
abstract class Batch[+A] private[containers]
  extends Equals
    with Immutable
    with Family[Batch[A]]
    with Index[A]
    with Deque[A] {
  
  /** Returns all but the first `lower` elements of this $collection.
    * @group Slicing */
  def drop(lower: Int): Batch[A]
  
  /** Returns the first `upper` elements of this $collection.
    * @group Slicing */
  def take(upper: Int): Batch[A]
  
  /** Returns a copy of this $collection with the given element appended.
    * @group Inserting */
  def append[B >: A](elem: B): Batch[B]
  
  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def prepend[B >: A](elem: B): Batch[B]
  
  /** Returns a copy of this $collection with the given element appended.
    * @group Inserting */
  def :+ [B >: A](elem: B): Batch[B] = append(elem)
  
  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def +: [B >: A](elem: B): Batch[B] = prepend(elem)
  
  protected override def stringPrefix: String = "Batch"
}

object Batch extends SeqFactory[Batch] {
  private[containers] object Empty extends Batch[Nothing] {
    override def isEmpty: Boolean = true
    
    override def length: Int = 0
    
    override def apply(index: Int): Nothing = throw new IndexOutOfBoundsException(index.toString)
    
    override def head: Nothing = throw new NoSuchElementException("head of empty batch")
    
    override def last: Nothing = throw new NoSuchElementException("last of empty batch")
    
    override def init: Batch[Nothing] = throw new UnsupportedOperationException("init of empty batch")
    
    override def tail: Batch[Nothing] = throw new UnsupportedOperationException("tail of empty batch")
    
    override def drop(lower: Int): Batch[Nothing] = this
    
    override def take(upper: Int): Batch[Nothing] = this
    
    override def append[B](elem: B): Batch[B] = {
      if (elem.isInstanceOf[Int]) new IntBatch1(elem.asInstanceOf[Int]).asInstanceOf[Batch[B]]
      else new RefBatch1(elem)
    }
    
    override def prepend[B](elem: B): Batch[B] = {
      if (elem.isInstanceOf[Int]) new IntBatch1(elem.asInstanceOf[Int]).asInstanceOf[Batch[B]]
      else new RefBatch1(elem)
    }
  }
  
  implicit override def Builder[A : TypeHint]
    : Builder[Any, A] { type State = Batch[A] } =
    new BatchBuilder
  
  override def empty[A : TypeHint]: Batch[A] = Empty
}

private[containers] final class BatchBuilder[A] extends Builder[Any, A] {
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
}
