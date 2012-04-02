/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

/** A discrete unary function sampled by reference on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with a sample array.
  * @tparam A       the sample type.
  * @param  array   The sample array.
  * @param  base    The offset of the first sample in the array.
  * @param  lower   The lower bound of the sampled domain.
  * @param  upper   The upper bound of the sampled domain.
  */
class RefImage1[A]
    (val array: Array[AnyRef], val base: Int)
    (val lower: Long, val upper: Long)
  extends Raster1[A] {
  
  assert(lower < upper)
  assert(base + (upper - lower + 1L) <= array.length)
  
  /** Constructs an un-initialized image on a given domain.
    * 
    * @param  lower   The lower bound of the sampled domain.
    * @param  upper   The upper bound of the sampled domain.
    */
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
