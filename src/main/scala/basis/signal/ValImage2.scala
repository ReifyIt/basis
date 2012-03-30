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
    (val min1: Long, val max1: Long, val min2: Long, val max2: Long)
    (implicit val struct: Struct[A])
  extends MutableImage2[A] {
  
  assert(min1 < max1)
  assert(min2 < max2)
  assert(baseAddress + stride * (max2 - min2 + 1L) <= data.size)
  
  def this(min1: Long, max1: Long, min2: Long, max2: Long)(implicit allocator: Allocator, struct: Struct[A]) =
    this(Data.alloc[A]((max1 - min1 + 1L) * (max2 - min2 + 1L)), 0L, struct.size * (max1 - min1 + 1L))(min1, max1, min2, max2)
  
  def apply(i: Long, j: Long): A = {
    if (i < min1 || i > max1) throw new IndexOutOfBoundsException(i.toString)
    if (j < min2 || j > max2) throw new IndexOutOfBoundsException(j.toString)
    struct.load(data, baseAddress + stride * (j - min2) + struct.size * (i - min1))
  }
  
  def update(i: Long, j: Long, sample: A) {
    if (i < min1 || i > max1) throw new IndexOutOfBoundsException(i.toString)
    if (j < min2 || j > max2) throw new IndexOutOfBoundsException(j.toString)
    struct.store(data, baseAddress + stride * (j - min2) + struct.size * (i - min1), sample)
  }
  
  override def translate(delta1: Long, delta2: Long): ValImage2[A] =
    new ValImage2[A](data, baseAddress, stride)(min1 + delta1, max1 + delta1, min2 + delta2, max2 + delta2)
}
