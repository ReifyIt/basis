/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** Contains an abstract memory model with struct typeclasses. */
package object memory {
  /** Returns an `Int` address advanced to a power-of-two alignment.
    * 
    * @param  alignment   the required alignment–forced to a power-of-two.
    * @param  address     the address to align.
    * @return the aligned address.
    */
  def align(alignment: Int)(address: Int): Int = {
    var n = alignment - 1
    n |= n >>> 1
    n |= n >>> 2
    n |= n >>> 4
    n |= n >>> 8
    n |= n >>> 16
    (address + n) & ~n
  }
  
  /** Returns a `Long` address advanced to a power-of-two alignment.
    * 
    * @param  alignment   the required alignment–forced to a power-of-two.
    * @param  address     the address to align.
    * @return the aligned address.
    */
  def align(alignment: Long)(address: Long): Long = {
    var n = alignment - 1L
    n |= n >>> 1
    n |= n >>> 2
    n |= n >>> 4
    n |= n >>> 8
    n |= n >>> 16
    n |= n >>> 32
    (address + n) & ~n
  }
  
  /** Returns the alignment of some value type. */
  @inline def alignOf[T](implicit struct: ValType[T]): Long = struct.alignment
  
  /** Returns the size of some value type. */
  @inline def sizeOf[T](implicit struct: ValType[T]): Long = struct.size
  
  /** Returns the implicit data type for some instance type.
    * 
    * @example {{{
    * scala> typeOf[(Int, Double)]
    * res0: basis.memory.DataType[(Int, Double)] = Row2(PaddedInt, PaddedDouble).project(size = 16, alignment = 8)
    * }}}
    * 
    * @tparam T         the instance type of the data type to get.
    * @param  datatype  the implicit data type itself.
    * @return the implicitly supplied data type.
    */
  @inline def typeOf[T](implicit datatype: DataType[T]): DataType[T] = datatype
}
