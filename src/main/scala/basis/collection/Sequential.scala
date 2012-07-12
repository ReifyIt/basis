/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Sequential[+A] extends Any with Iterable[A] {
  override def iterator: Iterator[A]
  
  def corresponds[B](that: Sequential[B])(p: (A, B) => Boolean): Boolean = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext) if (!p(these.next(), those.next())) return false
    !these.hasNext && !those.hasNext
  }
  
  override def eagerly: Sequencing[Any, A] = new Sequencing.Projecting[Any, A](this)
  
  override def lazily: Sequenced[A] = new Sequenced.Projected[A](this)
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Sequential[_]]
  
  override def equals(other: Any): Boolean = other match {
    case that: Sequential[_] => (that canEqual this) && (this sameAs that)
    case _ => false
  }
  
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    val iter = iterator
    var h = 1829453087
    var i = 0
    while (iter.hasNext) {
      h = mix(h, iter.next().##)
      i += 1
    }
    finalizeHash(h, i)
  }
}

object Sequential {
  abstract class Abstractly[+A] extends Iterable.Abstractly[A] with Sequential[A]
}
