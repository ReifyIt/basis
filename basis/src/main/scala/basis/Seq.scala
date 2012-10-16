/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An iterable sequence of elements. Import [[basis.collection.SeqOps]] to
  * extend this interface with a rich suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Container[A] {
  override type Self <: Seq[A]
  
  override def iterator: Iterator[A]
}

object Seq {
  /* implicit */ def Equal[A : Equal]: Equal[Seq[A]] = new SeqEqual[A]
  
  /* implicit */ def Hash[A : Hash]: Hash[Seq[A]] = new SeqHash[A]
  
  /* implicit */ def Show[A](implicit A: Show[A]): Show[Seq[A]] =
    new ContainerShow[A]("Seq")
}

private[basis] class SeqEqual[-A]
    (implicit protected val A: Equal[A])
  extends Equal[Seq[A]] {
  
  override def equal(xs: Seq[A], ys: Seq[A]): Boolean = {
    val iterX = xs.iterator
    val iterY = ys.iterator
    while (!iterX.isEmpty && !iterY.isEmpty) {
      if (!A.equal(iterX.head, iterY.head)) return false
      iterX.step()
      iterY.step()
    }
    iterX.isEmpty == iterY.isEmpty
  }
}

private[basis] class SeqHash[-A]
    (implicit override protected val A: Hash[A])
  extends SeqEqual[A] with Hash[Seq[A]] {
  
  override def hash(xs: Seq[A]): Int = {
    var h = 252152510
    val iter = xs.iterator
    while (!iter.isEmpty) {
      h = Hash.mix(h, A.hash(iter.head))
      iter.step()
    }
    Hash.mash(h)
  }
}
