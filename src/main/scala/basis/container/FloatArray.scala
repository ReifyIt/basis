/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class FloatArray(val array: Array[Float]) extends AnyVal with AnyArray[Float] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Float = array(index)
  
  @inline override def update(index: Int, value: Float): Unit = array(index) = value
  
  override def copy(length: Int = this.length): FloatArray = {
    val newArray = new Array[Float](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new FloatArray(newArray)
  }
}

object FloatArray {
  val Empty: FloatArray = FloatArray(0)
  
  @inline def apply(length: Int): FloatArray =
    new FloatArray(new Array[Float](length))
  
  final class Builder extends basis.collection.Builder[Any, Float] {
    override type Result = FloatArray
    
    private[this] var array: FloatArray = FloatArray.Empty
    
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
    
    override def += (value: Float) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: FloatArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = FloatArray.Empty
      aliased = true
      length = 0
    }
  }
}
