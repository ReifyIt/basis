/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An associated set of (key, value) pairs with unique keys. Import
  * [[basis.collection.MapOps]] to extend this interface with a full
  * suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  map
  */
trait Map[A, +T] extends Any with Container[(A, T)] {
  override type Self <: Map[A, T]
  
  /** Returns a new iterator over the (key, value) pairs of this $collection. */
  override def iterator: Iterator[(A, T)]
  
  /** Returns `true` if this $collection has no associations. */
  def isEmpty: Boolean
  
  /** Returns the number of associations in this $collection. */
  def size: Int
  
  /** Returns `true` if this $collection has a value associated with the given key. */
  def contains(key: A): Boolean
  
  /** Returns the value associated with the given key. */
  def apply(key: A): T
  
  /** Returns some value associated with the given key, or none if no association exists. */
  def get(key: A): Option[T]
  
  /** Returns a copy of this $collection with the given value associated with the given key. */
  def + [U >: T](key: A, value: U): Map[A, U]
  
  /** Returns a copy of this $collection without any value associated with the given key. */
  def - (key: A): Map[A, T]
}

private[basis] class MapShow[A, T]
    (name: String)(implicit A: Show[A], T: Show[T])
  extends Show[Map[A, T]] {
  
  override def show(map: Map[A, T])(implicit buffer: CharBuffer) {
    buffer.append(name)
    buffer += '('
    val iter = map.iterator
    if (!iter.isEmpty) {
      val entry = iter.head
      A.show(entry._1)(buffer)
      buffer.append(" -> ")
      T.show(entry._2)(buffer)
      iter.step()
      while (!iter.isEmpty) {
        buffer += ',' += ' '
        val entry = iter.head
        A.show(entry._1)(buffer)
        buffer.append(" -> ")
        T.show(entry._2)(buffer)
        iter.step()
      }
    }
    buffer += ')'
  }
}
