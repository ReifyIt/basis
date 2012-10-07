/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package container

object Array {
  import basis.data._
  import ValType._
  
  def apply[A](length: Int)(implicit typeA: MemType[A]): Array[A] = {
    (typeA match {
      case typeA: RefType[A]           => RefArray[A](length)
      case PackedByte                  => ByteArray(length)
      case PackedShort  | PaddedShort  => ShortArray(length)
      case PackedInt    | PaddedInt    => IntArray(length)
      case PackedLong   | PaddedLong   => LongArray(length)
      case PackedFloat  | PaddedFloat  => FloatArray(length)
      case PackedDouble | PaddedDouble => DoubleArray(length)
      case PackedBoolean               => BitArray(length)
      case typeA: ValType[A]           => ValArray[A](length)(typeA)
    }).asInstanceOf[Array[A]]
  }
  
  private[container] def expand(base: Int, size: Int): Int = {
    var n = scala.math.max(base, size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}
