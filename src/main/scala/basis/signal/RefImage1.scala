/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

/** A discrete unary function sampled by reference on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with a sample array.
  * @tparam A       the sample type.
  * @param  array   The sample array.
  * @param  base    The offset of the first sample in the array.
  * @param  domain  The bounded domain of the image. The lower bound of the
  *                 domain corresponds to the first sample in the array.
  */
class RefImage1[A]
    (val array: Array[AnyRef], val base: Int, val domain: Interval)
  extends Raster1[A] {
  
  assert(base + domain.size <= array.length)
  
  /** Constructs an un-initialized image on a given domain. */
  def this(domain: Interval) =
    this(new Array[AnyRef](domain.size.toInt), 0, domain)
  
  def apply(i: Long): A = {
    if (!domain.contains(i)) throw new IndexOutOfBoundsException(i.toString)
    array(base + (i - domain.lower).toInt).asInstanceOf[A]
  }
  
  def update(i: Long, sample: A) {
    if (!domain.contains(i)) throw new IndexOutOfBoundsException(i.toString)
    array(base + (i - domain.lower).toInt) = sample.asInstanceOf[AnyRef]
  }
  
  override def translate(delta: Long): RefImage1[A] =
    new RefImage1[A](array, base, domain + delta)
}
