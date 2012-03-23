/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** Contains a low-level memory abstraction and value typeclasses. */
package object memory {
  /** Aligns a value to a power-of-2 alignment.
    * 
    * @param  alignment   the required alignment–forced to a power-of-2.
    * @param  value       the value to align.
    * @return the aligned value.
    */
  def align(alignment: Long)(value: Long): Long = {
    var n = alignment - 1L
    n |= n >>> 1
    n |= n >>> 2
    n |= n >>> 4
    n |= n >>> 8
    n |= n >>> 16
    n |= n >>> 32
    (value + n) & ~n
  }
  
  /** Returns the alignment of the implicit value type of an instance type. */
  @inline def alignOf[T](implicit struct: Struct[T]): Long = struct.alignment
  
  /** Returns the size of the implicit value type of an instance type. */
  @inline def sizeOf[T](implicit struct: Struct[T]): Long = struct.size
}
