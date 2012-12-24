/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Virtual memory operations. */
package object memory {
  /** Returns an address aligned to a power-of-two alignment.
    * 
    * @param  base        the address to align.
    * @param  alignment   the required alignment.
    * @return the aligned address.
    */
  def align(base: Long, alignment: Long): Long =
    (base + (alignment - 1L)) & ~(alignment - 1L)
  
  /** Returns the alignment of a struct type. */
  def alignOf[T](implicit T: Struct[T]): Long = T.alignment
  
  /** Returns the size of a struct type. */
  def sizeOf[T](implicit T: Struct[T]): Long = T.size
  
  /** The native byte order of the virtual machine. */
  implicit val NativeEndian: Endianness = {
    if (java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN) BigEndian
    else if (java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN) LittleEndian
    else throw new MatchError(java.nio.ByteOrder.nativeOrder)
  }
}
