/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class LongArray(val array: Array[Long]) extends AnyVal with AnyArray[Long] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Long = array(index)
  
  @inline override def update(index: Int, value: Long): Unit = array(index) = value
  
  override def copy(length: Int = this.length): LongArray = {
    val newArray = new Array[Long](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new LongArray(newArray)
  }
}

object LongArray {
  val Empty: LongArray = LongArray(0)
  
  @inline def apply(length: Int): LongArray =
    new LongArray(new Array[Long](length))
  
  final class Builder extends basis.collection.Builder[Any, Long] {
    override type Result = LongArray
    
    private[this] var array: LongArray = LongArray.Empty
    
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
    
    override def += (value: Long) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: LongArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = LongArray.Empty
      aliased = true
      length = 0
    }
  }
}
