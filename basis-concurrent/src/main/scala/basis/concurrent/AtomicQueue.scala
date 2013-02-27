/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.concurrent

import basis.collections._
import basis.containers._
import basis.control._
import basis.runtime._

import scala.Predef.<:<

/** A concurrently mutable queue.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Measuring     1
  * @groupprio  Mutating      2
  * @groupprio  Traversing    3
  * @groupprio  Exporting     4
  * @groupprio  Classifying   5
  * 
  * @define collection  atomic queue
  */
final class AtomicQueue[A](@volatile private[concurrent] var queue: Batch[A])
  extends Equals
    with Mutable
    with Family[Queue[_]]
    with Seq[A] {
  
  import MetaAtomicQueue._
  
  def this() = this(Batch.empty[A])
  
  /** Returns `true` if this $collection doesn't currently contain any elements.
    * @group Measuring */
  def isEmpty: Boolean = queue.isEmpty
  
  /** Returns the current number of elements in this $collection.
    * @group Measuring */
  def length: Int = queue.length
  
  /** Pushes an element to the foot of this $collection.
    * @group Mutating */
  def push(elem: A) {
    var q = null: Batch[A]
    do q = queue
    while (!Unsafe.compareAndSwapObject(this, QueueOffset, q, q :+ elem))
  }
  
  /** Removes and returns the head of this $collection, if non-empty.
    * @group Mutating */
  def poll(): Maybe[A] = {
    var q = null: Batch[A]
    do q = queue
    while (!q.isEmpty && !Unsafe.compareAndSwapObject(this, QueueOffset, q, q.tail))
    if (!q.isEmpty) Bind(q.head) else Trap
  }
  
  /** Removes and returns the head of this $collection; returns `null` if empty.
    * @group Mutating */
  def pollOrNull()(implicit isNullable: Null <:< A): A = {
    var q = null: Batch[A]
    do q = queue
    while (!q.isEmpty && !Unsafe.compareAndSwapObject(this, QueueOffset, q, q.tail))
    if (!q.isEmpty) q.head else null.asInstanceOf[A]
  }
  
  override def traverse(f: A => Unit): Unit = queue traverse f
  
  override def iterator: Iterator[A] = queue.iterator
  
  /** Returns the immutable queue underlying this $collection.
    * @group Exporting */
  def toBatch: Batch[A] = queue
  
  protected override def stringPrefix: String = "AtomicQueue"
}

/** A factory for [[AtomicQueue atomic queues]]. */
object AtomicQueue extends SeqFactory[AtomicQueue, TypeHint] {
  implicit override def Builder[A : TypeHint]
    : Builder[A] { type Scope = AtomicQueue[_]; type State = AtomicQueue[A] } =
    new AtomicQueueBuilder[A]
  
  override def toString: String = "AtomicQueue"
}

private[concurrent] final class AtomicQueueBuilder[A] extends Builder[A] {
  override type Scope = AtomicQueue[_]
  override type State = AtomicQueue[A]
  private[this] var queue: Batch[A] = Batch.empty[A]
  override def append(elem: A): Unit = queue = queue :+ elem
  override def expect(count: Int): this.type = this
  override def state: AtomicQueue[A] = new AtomicQueue(queue)
  override def clear(): Unit = queue = Batch.empty[A]
  override def toString: String = "AtomicQueueBuilder"
}
