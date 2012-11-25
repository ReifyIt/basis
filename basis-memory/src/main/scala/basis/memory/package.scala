/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** Low-level memory operations. */
package object memory {
  /** Returns a given address advanced to a power-of-two alignment.
    * 
    * @param  alignment   the required alignment.
    * @param  address     the address to align.
    * @return the aligned address.
    */
  def align(base: Long, alignment: Long): Long =
    (base + (alignment - 1L)) & ~(alignment - 1L)
  
  /** Returns the alignment of type's implicit `ValType`. */
  def alignOf[T](implicit struct: ValType[T]): Long = struct.alignment
  
  /** Returns the size of a type's implicit `ValType`. */
  def sizeOf[T](implicit struct: ValType[T]): Long = struct.size
  
  /** Returns the native byte order of the virtual machine. */
  implicit val NativeEndian: ByteOrder = {
    if (java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN) BigEndian
    else if (java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN) LittleEndian
    else throw new MatchError(java.nio.ByteOrder.nativeOrder)
  }
}
