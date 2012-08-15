/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class CharArray(val array: Array[Char]) extends AnyVal with AnyArray[Char] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Char = array(index)
  
  @inline override def update(index: Int, value: Char): Unit = array(index) = value
  
  override def copy(length: Int = this.length): CharArray = {
    val newArray = new Array[Char](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new CharArray(newArray)
  }
}

object CharArray {
  val Empty: CharArray = CharArray(0)
  
  @inline def apply(length: Int): CharArray =
    new CharArray(new Array[Char](length))
  
  final class Builder extends basis.collection.Builder[Any, Char] {
    override type Result = CharArray
    
    private[this] var array: CharArray = CharArray.Empty
    
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
    
    override def += (value: Char) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: CharArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = CharArray.Empty
      aliased = true
      length = 0
    }
  }
}
