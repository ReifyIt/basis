/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection
package mutable

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder
import scala.collection.parallel.mutable.ParArray

import basis.collection.generic._

/** An array that stores its elements by reference.
  * 
  * @author Chris Sachs
  */
class RefArray[A](val array: Array[AnyRef], val base: Int, override val length: Int)
  extends RawArray[A]
    with GenericTraversableTemplate[A, RawArray]
    with mutable.IndexedSeqLike[A, RefArray[A]]
    with mutable.IndexedSeqOptimized[A, RefArray[A]]
    with CustomParallelizable[A, ParArray[A]] {
  
  require(base + length <= array.length)
  
  def this(length: Int) = this(new Array[AnyRef](length), 0, length)
  
  def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    array(base + index).asInstanceOf[A]
  }
  
  def update(index: Int, value: A) {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    array(base + index) = value.asInstanceOf[AnyRef]
  }
  
  override def foreach[U](f: A => U) {
    var i = base
    while (i < length) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }
  
  override def copyToArray[B >: A](dest: Array[B], start: Int, count: Int): Unit =
    Array.copy(array, base, dest, start, math.min(count, length))
  
  override def par: ParArray[A] = {
    if (base == 0) ParArray.handoff(array.asInstanceOf[Array[A]], length)
    else {
      val copy = new Array[AnyRef](length)
      Array.copy(array, base, copy, 0, length)
      ParArray.handoff(copy.asInstanceOf[Array[A]], length)
    }
  }
  
  override protected def newBuilder: Builder[A, RefArray[A]] = RefArray.newBuilder[A]
  
  override def stringPrefix: String = "RefArray"
}

/** A factory for `RefArray` sequences. */
object RefArray extends RefSeqFactory[RefArray] {
  implicit def canBuildFrom[A]: CanBuildFrom[RefArray[_], A, RefArray[A]] =
    RefBuilderFactory.asInstanceOf[RefBuilderFactory[A]]
  
  def newBuilder[A]: Builder[A, RefArray[A]] =
    new RefBuilder[A]
  
  protected class RefBuilder[A] extends BuilderProxy[A, RefArray[A]] {
    val self = new RefBuffer[A]
    def result() = new RefArray[A](self.array, 0, self.length)
  }
}
