/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.memory._

class ValImage1[A]
    (val data: Data, val baseAddress: Long)
    (val min: Long, val max: Long)
    (implicit val struct: Struct[A])
  extends MutableImage1[A] {
  
  assert(min < max)
  assert(baseAddress + struct.size * (max - min + 1L) <= data.size)
  
  def this(min: Long, max: Long)(implicit allocator: Allocator, struct: Struct[A]) =
    this(Data.alloc[A](max - min + 1L), 0L)(min, max)
  
  def apply(i: Long): A = {
    if (i < min || i > max) throw new IndexOutOfBoundsException(i.toString)
    struct.load(data, baseAddress + struct.size * (i - min))
  }
  
  def update(i: Long, sample: A) {
    if (i < min || i > max) throw new IndexOutOfBoundsException(i.toString)
    struct.store(data, baseAddress + struct.size * (i - min), sample)
  }
  
  override def translate(delta: Long): ValImage1[A] =
    new ValImage1[A](data, baseAddress)(min + delta, max + delta)
}
