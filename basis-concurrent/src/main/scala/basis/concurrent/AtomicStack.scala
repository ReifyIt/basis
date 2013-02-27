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

/** A concurrently mutable stack.
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
  * @define collection  atomic stack
  */
final class AtomicStack[A](@volatile private[concurrent] var stack: List[A])
  extends Equals
    with Mutable
    with Family[Stack[_]]
    with ListLike[A]
    with Seq[A] {
  
  import MetaAtomicStack._
  
  def this() = this(Nil)
  
  /** Returns `true` if this $collection doesn't currently contain any elements.
    * @group Measuring */
  def isEmpty: Boolean = stack.isEmpty
  
  /** Pushes an element to the head of this $collection.
    * @group Mutating */
  def push(elem: A) {
    var s = null: List[A]
    do s = stack
    while (!Unsafe.compareAndSwapObject(this, StackOffset, s, elem :: s))
  }
  
  /** Removes and returns the head of this $collection, if non-empty.
    * @group Mutating */
  def pop(): Maybe[A] = {
    var s = null: List[A]
    do s = stack
    while (!s.isEmpty && !Unsafe.compareAndSwapObject(this, StackOffset, s, s.tail))
    if (!s.isEmpty) Bind(s.head) else Trap
  }
  
  /** Removes and returns the head of this $collection; returns `null` if empty.
    * @group Mutating */
  def popOrNull()(implicit isNullable: Null <:< A): A = {
    var s = null: List[A]
    do s = stack
    while (!s.isEmpty && !Unsafe.compareAndSwapObject(this, StackOffset, s, s.tail))
    if (!s.isEmpty) s.head else null.asInstanceOf[A]
  }
  
  override def traverse(f: A => Unit): Unit = stack traverse f
  
  override def iterator: Iterator[A] = stack.iterator
  
  /** Returns the immutable stack underlying this $collection.
    * @group Exporting */
  override def toList: List[A] = stack
  
  protected override def stringPrefix: String = "AtomicStack"
}

/** A factory for [[AtomicStack atomic stacks]]. */
object AtomicStack extends SeqFactory[AtomicStack, TypeHint] {
  implicit override def Builder[A : TypeHint]
    : Builder[A] { type Scope = AtomicStack[_]; type State = AtomicStack[A] } =
    new AtomicStackBuilder[A]
  
  override def toString: String = "AtomicStack"
}

private[concurrent] final class AtomicStackBuilder[A] extends ListBuffer[A] {
  override type Scope = AtomicStack[_]
  override type State = AtomicStack[A]
  override def state: AtomicStack[A] = new AtomicStack(toList)
  override def toString: String = "AtomicStackBuilder"
}
