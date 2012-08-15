/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class IntArray(val array: Array[Int]) extends AnyVal with AnyArray[Int] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Int = array(index)
  
  @inline override def update(index: Int, value: Int): Unit = array(index) = value
  
  override def copy(length: Int = this.length): IntArray = {
    val newArray = new Array[Int](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new IntArray(newArray)
  }
}

object IntArray {
  val Empty: IntArray = IntArray(0)
  
  @inline def apply(length: Int): IntArray =
    new IntArray(new Array[Int](length))
  
  final class Builder extends basis.collection.Builder[Any, Int] {
    override type Result = IntArray
    
    private[this] var array: IntArray = IntArray.Empty
    
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
    
    override def += (value: Int) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: IntArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = IntArray.Empty
      aliased = true
      length = 0
    }
  }
}
