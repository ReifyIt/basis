/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

/** A discrete binary function sampled by reference on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with a sample array.
  * @tparam A       the sample type.
  * @param  array   The sample array.
  * @param  stride  The number of samples in the first dimension.
  * @param  base    The offset of the first sample in the array.
  * @param  lower1  The lower bound of the domain's first component.
  * @param  upper1  The upper bound of the domain's first component.
  * @param  lower2  The lower bound of the domain's second component.
  * @param  upper2  The upper bound of the domain's second component.
  */
class RefImage2[A]
    (val array: Array[AnyRef], val base: Int, val stride: Int)
    (val lower1: Long, val upper1: Long, val lower2: Long, val upper2: Long)
  extends Raster2[A] {
  
  assert(lower1 < upper1)
  assert(lower2 < upper2)
  assert(base + stride * (upper2 - lower2 + 1L) <= array.length)
  
  /** Constructs an un-initialized image on a given domain.
    * 
    * @param  lower1  The lower bound of the domain's first component.
    * @param  upper1  The upper bound of the domain's first component.
    * @param  lower2  The lower bound of the domain's second component.
    * @param  upper2  The upper bound of the domain's second component.
    */
  def this(lower1: Long, upper1: Long, lower2: Long, upper2: Long) =
    this(new Array[AnyRef](((upper1 - lower1 + 1L) * (upper2 - lower2 + 1L)).toInt), 0, (upper1 - lower1 + 1L).toInt)(lower1, upper1, lower2, upper2)
  
  def apply(i: Long, j: Long): A = {
    if (i < lower1 || i > upper1) throw new IndexOutOfBoundsException(i.toString)
    if (j < lower2 || j > upper2) throw new IndexOutOfBoundsException(j.toString)
    array(base + stride * (j - lower2).toInt + (i - lower1).toInt).asInstanceOf[A]
  }
  
  def update(i: Long, j: Long, sample: A) {
    if (i < lower1 || i > upper1) throw new IndexOutOfBoundsException(i.toString)
    if (j < lower2 || j > upper2) throw new IndexOutOfBoundsException(j.toString)
    array(base + stride * (j - lower2).toInt + (i - lower1).toInt) = sample.asInstanceOf[AnyRef]
  }
  
  override def translate(delta1: Long, delta2: Long): RefImage2[A] =
    new RefImage2[A](array, base, stride)(lower1 + delta1, upper1 + delta1, lower2 + delta2, upper2 + delta2)
}
