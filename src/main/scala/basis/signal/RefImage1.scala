/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

class RefImage1[A]
    (val array: Array[AnyRef], val base: Int)
    (val min: Long, val max: Long)
  extends MutableImage1[A] {
  
  assert(min < max)
  assert(base + (max - min + 1L) <= array.length)
  
  def this(min: Long, max: Long) =
    this(new Array[AnyRef]((max - min + 1L).toInt), 0)(min, max)
  
  def apply(i: Long): A = {
    if (i < min || i > max) throw new IndexOutOfBoundsException(i.toString)
    array(base + (i - min).toInt).asInstanceOf[A]
  }
  
  def update(i: Long, sample: A) {
    if (i < min || i > max) throw new IndexOutOfBoundsException(i.toString)
    array(base + (i - min).toInt) = sample.asInstanceOf[AnyRef]
  }
  
  override def translate(delta: Long): RefImage1[A] =
    new RefImage1[A](array, base)(min + delta, max + delta)
}
