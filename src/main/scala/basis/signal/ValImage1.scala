/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.memory._

/** A discrete unary function sampled by value on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an image with sample data.
  * @tparam T             the sample type.
  * @param  data          The sample data.
  * @param  baseAddress   The address of the first sample in the data.
  * @param  lower         The lower bound of the sampled domain.
  * @param  upper         The upper bound of the sampled domain.
  * @param  struct        The sample struct.
  */
class ValImage1[A]
    (val data: Data, val baseAddress: Long)
    (val lower: Long, val upper: Long)
    (implicit val struct: Struct[A])
  extends Raster1[A] {
  
  assert(lower < upper)
  assert(baseAddress + struct.size * (upper - lower + 1L) <= data.size)
  
  /** Constructs an un-initialized image on a given domain.
    * 
    * @param  lower   The lower bound of the sampled domain.
    * @param  upper   The upper bound of the sampled domain.
    */
  def this(lower: Long, upper: Long)(implicit allocator: Allocator, struct: Struct[A]) =
    this(Data.alloc[A](upper - lower + 1L), 0L)(lower, upper)
  
  def apply(i: Long): A = {
    if (i < lower || i > upper) throw new IndexOutOfBoundsException(i.toString)
    struct.load(data, baseAddress + struct.size * (i - lower))
  }
  
  def update(i: Long, sample: A) {
    if (i < lower || i > upper) throw new IndexOutOfBoundsException(i.toString)
    struct.store(data, baseAddress + struct.size * (i - lower), sample)
  }
  
  override def translate(delta: Long): ValImage1[A] =
    new ValImage1[A](data, baseAddress)(lower + delta, upper + delta)
}
