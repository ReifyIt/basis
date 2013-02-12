/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A stateful collection traverser. An iterator steps through each element
  * of a collection, one element per `step()`, until `isEmpty` returns `true`.
  * Backtracking algorithms can `dup` an iterator's state and resume it after
  * advancing the original iterator.
  * 
  * ==Iterator states==
  *
  * Each `step()` forwards the iterator into one of three states:
  * ''buffered'', ''empty'', or ''done''.
  * 
  *  - In the ''buffered'' state, `head` returns the current element,
  *    and both `isEmpty` and `isDone` return `false`.
  *  - In the ''empty'' state, `head` is undefined, `isEmpty` returns `true`,
  *    and `isDone` returns `false`.
  *  - In the ''done'' state, like the ''empty'' state, `head` is undefined,
  *    and both `isEmpty` and `isDone` return `true`.
  * 
  * The distinct ''empty'' and ''done'' states facilitate low-overhead
  * "chunked" iterator applications, such as iteratees.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Collections
  * 
  * @groupprio  Examining     1
  * @groupprio  Traversing    2
  * @groupprio  Classifying   3
  * 
  * @define collection  iterator
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralEnumeratorOps GeneralIteratorOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictEnumeratorOps StrictIteratorOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictEnumeratorOps NonStrictIteratorOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Iterator[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Iterator[_]] with Enumerator[A] {
  
  /** Returns `true` when this $collection has reached a sentinel element.
    * @group Examining */
  def isDone: Boolean = false
  
  /** Returns `true` when this $collection has no more elements.
    * @group Examining */
  def isEmpty: Boolean
  
  /** Returns the currently buffered element.
    * @group Examining */
  def head: A
  
  /** Advances this $collection to its next state.
    * @group Traversing */
  def step(): Unit
  
  /** Returns a duplicate $collection with identical but independent state.
    * @group Traversing */
  def dup: Iterator[A]
  
  override def traverse(f: A => Unit): Unit =
    while (!isEmpty) { f(head); step() }
}
