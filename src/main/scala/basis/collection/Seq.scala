/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Seq[+A] extends Any with Equals with Iterable[A] {
  override def iterator: Iterator[A]
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Seq[_]]
  
  override def equals(other: Any): Boolean = other match {
    case that: Seq[_] =>
      val these = this.iterator
      val those = that.iterator
      while (these.hasNext && those.hasNext) if (these.next() != those.next()) return false
      !these.hasNext && !those.hasNext
    case _ => false
  }
  
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    var h = 2588263
    var i = 0
    val iter = iterator
    while (iter.hasNext) {
      h = mix(h, iter.next().##)
      i += 1
    }
    finalizeHash(h, i)
  }
}

object Seq {
  import scala.language.implicitConversions
  
  @inline implicit def SeqOps[A](self: Seq[A]): SeqOps[self.Kind, A] =
    new SeqOps[self.Kind, A](self)
}
