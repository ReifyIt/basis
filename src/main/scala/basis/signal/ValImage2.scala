/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._
import basis.memory._

/** A discrete binary function sampled by value on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with sample data.
  * @tparam A             the sample type.
  * @param  data          The sample data.
  * @param  baseAddress   The address of the first sample in the data.
  * @param  domain        The bounded domain of this image.
  * @param  struct        The sample struct.
  */
class ValImage2[A]
    (val data: Data, val baseAddress: Long, val domain: IntervalZ2)
    (implicit val struct: Struct[A])
  extends Raster2[A] {
  
  assert(baseAddress + struct.size * domain.size <= data.size)
  
  /** The size in bytes of the ''x''-dimension. */
  protected val stride: Long = struct.size * domain.x.size
  
  /** Constructs an un-initialized image on a given domain. */
  def this(domain: IntervalZ2)(implicit allocator: Allocator, struct: Struct[A]) =
    this(Data.alloc[A](domain.size), 0L, domain)
  
  def apply(i: Long, j: Long): A = {
    if (!domain.contains(i, j)) throw new IndexOutOfBoundsException(i.toString +", "+ j.toString)
    struct.load(data, baseAddress + stride * (j - domain.y.lower) + struct.size * (i - domain.x.lower))
  }
  
  def update(i: Long, j: Long, sample: A) {
    if (!domain.contains(i, j)) throw new IndexOutOfBoundsException(i.toString +", "+ j.toString)
    struct.store(data, baseAddress + stride * (j - domain.y.lower) + struct.size * (i - domain.x.lower), sample)
  }
  
  override def translate(delta: VectorZ2): ValImage2[A] =
    new ValImage2[A](data, baseAddress, domain + delta)
}
