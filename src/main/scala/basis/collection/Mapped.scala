/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Mapped[K, +V] extends Any with Iterated[(K, V)] with Mappable[K, V] {
  override def iterator: Iterator[(K, V)]
  
  override def get(key: K): Option[V]
  
  def + [U >: V](entry: (K, U))(implicit builder: Collector[Scope, (K, U)]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) builder += iter.next()
    if (!contains(entry._1)) builder += entry
    builder.result
  }
  
  def - (key: K)(implicit builder: Collector[Scope, (K, V)]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) {
      val entry = iter.next()
      if (entry._1 != key) builder += entry
    }
    builder.result
  }
  
  override def eagerly: Mapped[K, V] = this
}

private[basis] object Mapped {
  private[basis] abstract class Abstractly[K, +V] extends Mappable.Abstractly[K, V] with Mapped[K, V]
  
  private[basis] final class Projected[K, +V](self: Mappable[K, V]) extends Abstractly[K, V] {
    override def iterator: Iterator[(K, V)] = self.iterator
    override def get(key: K): Option[V] = self.get(key)
  }
}
