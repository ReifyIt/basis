/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** An allocator for `Data` backed by an array.
  * 
  * @author Chris Sachs
  * 
  * @tparam V   the element type of allocated array data.
  * @see  [[basis.memory.Data]]
  */
trait ArrayAllocator[V] extends Allocator {
  override def alloc[T](count: Long)(implicit unit: Struct[T]): ArrayData[V]
  
  override def apply(size: Long): ArrayData[V]
  
  /** Returns a data object backed by the given array. */
  def wrap(array: Array[V]): ArrayData[V]
}
