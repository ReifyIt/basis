/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class ByteArray(val array: Array[Byte]) extends AnyVal with AnyArray[Byte] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Byte = array(index)
  
  @inline override def update(index: Int, value: Byte): Unit = array(index) = value
  
  override def copy(length: Int = this.length): ByteArray = {
    val newArray = new Array[Byte](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new ByteArray(newArray)
  }
}

object ByteArray {
  val Empty: ByteArray = ByteArray(0)
  
  @inline def apply(length: Int): ByteArray =
    new ByteArray(new Array[Byte](length))
  
  final class Builder extends basis.collection.Builder[Any, Byte] {
    override type Result = ByteArray
    
    private[this] var array: ByteArray = ByteArray.Empty
    
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
    
    override def += (value: Byte) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: ByteArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = ByteArray.Empty
      aliased = true
      length = 0
    }
  }
}
