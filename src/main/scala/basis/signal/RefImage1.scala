/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

class RefImage1[A]
    (val array: Array[AnyRef], val base: Int)
    (val lower: Long, val upper: Long)
  extends Raster1[A] {
  
  assert(lower < upper)
  assert(base + (upper - lower + 1L) <= array.length)
  
  def this(lower: Long, upper: Long) =
    this(new Array[AnyRef]((upper - lower + 1L).toInt), 0)(lower, upper)
  
  def apply(i: Long): A = {
    if (i < lower || i > upper) throw new IndexOutOfBoundsException(i.toString)
    array(base + (i - lower).toInt).asInstanceOf[A]
  }
  
  def update(i: Long, sample: A) {
    if (i < lower || i > upper) throw new IndexOutOfBoundsException(i.toString)
    array(base + (i - lower).toInt) = sample.asInstanceOf[AnyRef]
  }
  
  override def translate(delta: Long): RefImage1[A] =
    new RefImage1[A](array, base)(lower + delta, upper + delta)
}
