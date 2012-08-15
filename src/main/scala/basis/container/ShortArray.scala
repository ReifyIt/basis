/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class ShortArray(val array: Array[Short]) extends AnyVal with AnyArray[Short] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Short = array(index)
  
  @inline override def update(index: Int, value: Short): Unit = array(index) = value
  
  override def copy(length: Int = this.length): ShortArray = {
    val newArray = new Array[Short](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new ShortArray(newArray)
  }
}

object ShortArray {
  val Empty: ShortArray = ShortArray(0)
  
  @inline def apply(length: Int): ShortArray =
    new ShortArray(new Array[Short](length))
  
  final class Builder extends basis.collection.Builder[Any, Short] {
    override type Result = ShortArray
    
    private[this] var array: ShortArray = ShortArray.Empty
    
    private[this] var aliased: Boolean = true
    
    private[this] var length: Int = 0
    
    private[this] def prepare(size: Int) {
      if (aliased || size > array.length) {
        array = array.copy(basis.collection.Builder.expand(16, size))
        aliased = false
      }
    }
    
    override def expect(count: Int) {
      if (length + count > array.length) {
        array = array.copy(length + count)
        aliased = false
      }
    }
    
    override def += (value: Short) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: ShortArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = ShortArray.Empty
      aliased = true
      length = 0
    }
  }
}
