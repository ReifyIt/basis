/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package container

final class DoubleBuffer extends Buffer[Array[_], Double] {
  override type State = DoubleArray
  
  private[this] var array: DoubleArray = DoubleArray.Empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      array = array.copy(Array.expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: Double): this.type = {
    prepare(length + 1)
    array(length) = value
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    this
  }
  
  override def check: DoubleArray = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = DoubleArray.Empty
    aliased = true
    length = 0
  }
}
