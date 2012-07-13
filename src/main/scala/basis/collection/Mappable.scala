/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Mappable[K, +V] extends Any with Iterable[(K, V)] {
  override def iterator: Iterator[(K, V)]
  
  def get(key: K): Option[V]
  
  def getOrElse[U >: V](key: K, default: => U): U = get(key) match {
    case Some(value) => value
    case None => default
  }
  
  def apply(key: K): V = get(key) match {
    case Some(value) => value
    case None => throw new NoSuchElementException("key not found: "+ key)
  }
  
  def contains(key: K): Boolean = get(key).isDefined
  
  override def eagerly: Mapped[K, V] = new Mapped.Projected[K, V](this)
  
  override def lazily: Mapping[K, V] = new Mapping.Projecting[K, V](this)
}

private[basis] object Mappable {
  private[basis] abstract class Abstractly[K, +V] extends Iterable.Abstractly[(K, V)] with Mappable[K, V]
}
