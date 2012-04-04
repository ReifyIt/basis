/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

/** A discrete binary function sampled by reference on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with a sample array.
  * @tparam A       the sample type.
  * @param  array   The sample array.
  * @param  base    The offset of the first sample in the array.
  * @param  domain  The bounded domain of this image.
  */
class RefImage2[A]
    (val array: Array[AnyRef], val base: Int, val domain: IntervalZ2)
  extends Raster2[A] {
  
  assert(base + domain.size <= array.length)
  
  /** The number of samples in the ''x''-dimension. */
  protected val stride: Int = domain.x.size.toInt
  
  /** Constructs an un-initialized image on a given domain. */
  def this(domain: IntervalZ2) =
    this(new Array[AnyRef](domain.size.toInt), 0, domain)
  
  def apply(i: Long, j: Long): A = {
    if (!domain.contains(i, j)) throw new IndexOutOfBoundsException(i.toString +", "+ j.toString)
    array(base + stride * (j - domain.y.lower).toInt + (i - domain.x.lower).toInt).asInstanceOf[A]
  }
  
  def update(i: Long, j: Long, sample: A) {
    if (!domain.contains(i, j)) throw new IndexOutOfBoundsException(i.toString +", "+ j.toString)
    array(base + stride * (j - domain.y.lower).toInt + (i - domain.x.lower).toInt) = sample.asInstanceOf[AnyRef]
  }
  
  override def translate(delta: VectorZ2): RefImage2[A] =
    new RefImage2[A](array, base, domain + delta)
}
