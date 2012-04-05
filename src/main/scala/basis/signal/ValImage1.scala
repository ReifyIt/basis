/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._
import basis.memory._

/** A discrete unary function sampled by value on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with sample data.
  * @tparam A             the sample type.
  * @param  data          The sample data.
  * @param  baseAddress   The address of the first sample in the data.
  * @param  domain        The bounded domain of the image. The lower bound of
  *                       the domain corresponds to the first data sample.
  * @param  struct        The sample struct.
  */
class ValImage1[A]
    (val data: Data, val baseAddress: Long, val domain: IntervalZ1)
    (implicit val struct: Struct[A])
  extends Raster1[A] {
  
  assert(baseAddress + struct.size * domain.size <= data.size)
  
  /** Constructs an un-initialized image on a given domain. */
  def this(domain: IntervalZ1)(implicit allocator: Allocator, struct: Struct[A]) =
    this(Data.alloc[A](domain.size), 0L, domain)
  
  def apply(i: Long): A = {
    if (!domain.contains(i)) throw new IndexOutOfBoundsException(i.toString)
    struct.load(data, baseAddress + struct.size * (i - domain.lower))
  }
  
  def update(i: Long, sample: A) {
    if (!domain.contains(i)) throw new IndexOutOfBoundsException(i.toString)
    struct.store(data, baseAddress + struct.size * (i - domain.lower), sample)
  }
  
  override def translate(delta: Long): ValImage1[A] =
    new ValImage1[A](data, baseAddress, domain + delta)
}
