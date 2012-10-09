/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

final class RefBuffer[A] extends Buffer[Array[_], A] {
  override type State = RefArray[A]
  
  private[this] var array: RefArray[A] = RefArray.empty[A]
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      array = array.copy(Array.expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: A): this.type = {
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
  
  override def check: RefArray[A] = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = RefArray.empty[A]
    aliased = true
    length = 0
  }
}
