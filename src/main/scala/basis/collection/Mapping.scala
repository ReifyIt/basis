/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Mapping[K, +V] extends Any with Iterating[(K, V)] with Mappable[K, V] {
  import Mapping._
  
  override def iterator: Iterator[(K, V)]
  
  override def get(key: K): Option[V]
  
  def + [U >: V](entry: (K, U)): Mapping[K, U] = new Including[K, U](this, entry)
  
  def - (key: K): Mapping[K, V] = new Excluding[K, V](this, key)
  
  override def lazily: Mapping[K, V] = this
}

private[basis] object Mapping {
  private[basis] abstract class Abstractly[K, +V] extends Mappable.Abstractly[K, V] with Mapping[K, V]
  
  private[basis] final class Projecting[K, +V](self: Mappable[K, V]) extends Abstractly[K, V] {
    override def iterator: Iterator[(K, V)] = self.iterator
    override def get(key: K): Option[V] = self.get(key)
    override def + [U >: V](entry: (K, U)): Mapping[K, U] = new Including[K, U](self, entry)
    override def - (key: K): Mapping[K, V] = new Excluding[K, V](self, key)
  }
  
  private[basis] final class Including[K, +V](self: Mappable[K, V], entry: (K, V)) extends Abstractly[K, V] {
    override def iterator: Iterator[(K, V)] = self.iterator :+ entry
    override def get(key: K): Option[V] = if (entry._1 == key) Some(entry._2) else self.get(key)
  }
  
  private[basis] final class Excluding[K, +V](self: Mappable[K, V], key: K) extends Abstractly[K, V] {
    override def iterator: Iterator[(K, V)] = self.iterator.filter(_._1 != key)
    override def get(key: K): Option[V] = if (this.key == key) None else self.get(key)
  }
}
