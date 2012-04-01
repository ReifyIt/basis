/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.memory._

class ValImage2[A]
    (val data: Data, val baseAddress: Long, val stride: Long)
    (val lower1: Long, val upper1: Long, val lower2: Long, val upper2: Long)
    (implicit val struct: Struct[A])
  extends Raster2[A] {
  
  assert(lower1 < upper1)
  assert(lower2 < upper2)
  assert(baseAddress + stride * (upper2 - lower2 + 1L) <= data.size)
  
  def this(lower1: Long, upper1: Long, lower2: Long, upper2: Long)(implicit allocator: Allocator, struct: Struct[A]) =
    this(Data.alloc[A]((upper1 - lower1 + 1L) * (upper2 - lower2 + 1L)), 0L, struct.size * (upper1 - lower1 + 1L))(lower1, upper1, lower2, upper2)
  
  def apply(i: Long, j: Long): A = {
    if (i < lower1 || i > upper1) throw new IndexOutOfBoundsException(i.toString)
    if (j < lower2 || j > upper2) throw new IndexOutOfBoundsException(j.toString)
    struct.load(data, baseAddress + stride * (j - lower2) + struct.size * (i - lower1))
  }
  
  def update(i: Long, j: Long, sample: A) {
    if (i < lower1 || i > upper1) throw new IndexOutOfBoundsException(i.toString)
    if (j < lower2 || j > upper2) throw new IndexOutOfBoundsException(j.toString)
    struct.store(data, baseAddress + stride * (j - lower2) + struct.size * (i - lower1), sample)
  }
  
  override def translate(delta1: Long, delta2: Long): ValImage2[A] =
    new ValImage2[A](data, baseAddress, stride)(lower1 + delta1, upper1 + delta1, lower2 + delta2, upper2 + delta2)
}
