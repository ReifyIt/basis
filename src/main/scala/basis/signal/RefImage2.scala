/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

class RefImage2[A]
    (val array: Array[AnyRef], val base: Int, val stride: Int)
    (val min1: Long, val max1: Long, val min2: Long, val max2: Long)
  extends MutableImage2[A] {
  
  assert(min1 < max1)
  assert(min2 < max2)
  assert(base + stride * (max2 - min2 + 1L) <= array.length)
  
  def this(min1: Long, max1: Long, min2: Long, max2: Long) =
    this(new Array[AnyRef](((max1 - min1 + 1L) * (max2 - min2 + 1L)).toInt), 0, (max1 - min1 + 1L).toInt)(min1, max1, min2, max2)
  
  def apply(i: Long, j: Long): A = {
    if (i < min1 || i > max1) throw new IndexOutOfBoundsException(i.toString)
    if (j < min2 || j > max2) throw new IndexOutOfBoundsException(j.toString)
    array(base + stride * (j - min2).toInt + (i - min1).toInt).asInstanceOf[A]
  }
  
  def update(i: Long, j: Long, sample: A) {
    if (i < min1 || i > max1) throw new IndexOutOfBoundsException(i.toString)
    if (j < min2 || j > max2) throw new IndexOutOfBoundsException(j.toString)
    array(base + stride * (j - min2).toInt + (i - min1).toInt) = sample.asInstanceOf[AnyRef]
  }
  
  override def translate(delta1: Long, delta2: Long): RefImage2[A] =
    new RefImage2[A](array, base, stride)(min1 + delta1, max1 + delta1, min2 + delta2, max2 + delta2)
}
