/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A double-ended queue. Deques efficiently decompose into a `head` element
  * and a `tail` deque, or a `body` deque and a `foot` element.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Collections
  * 
  * @groupprio  Measuring     1
  * @groupprio  Decomposing   2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
  * 
  * @define collection  deque
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralDequeOps GeneralDequeOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictDequeOps StrictDequeOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictDequeOps NonStrictDequeOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Deque[@specialized(Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Deque[_]] with Stack[A] {
  
  override def head: A
  
  override def tail: Deque[A]
  
  /** Returns all elements except the last of this non-empty $collection.
    * @group Decomposing */
  def body: Deque[A]
  
  /** Returns the last element of this non-empty $collection.
    * @group Decomposing */
  def foot: A
}
