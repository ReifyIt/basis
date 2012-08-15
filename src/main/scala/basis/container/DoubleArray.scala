/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class DoubleArray(val array: Array[Double]) extends AnyVal with AnyArray[Double] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): Double = array(index)
  
  @inline override def update(index: Int, value: Double): Unit = array(index) = value
  
  override def copy(length: Int = this.length): DoubleArray = {
    val newArray = new Array[Double](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new DoubleArray(newArray)
  }
}

object DoubleArray {
  val Empty: DoubleArray = DoubleArray(0)
  
  @inline def apply(length: Int): DoubleArray =
    new DoubleArray(new Array[Double](length))
  
  final class Builder extends basis.collection.Builder[Any, Double] {
    override type Result = DoubleArray
    
    private[this] var array: DoubleArray = DoubleArray.Empty
    
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
    
    override def += (value: Double) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: DoubleArray = {
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
}
