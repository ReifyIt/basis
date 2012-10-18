/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class DoubleArray(val array: scala.Array[Double]) extends AnyVal with Array[Double] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Double = array(index)
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update(index: Int, value: Double): DoubleArray = {
    val newArray = array.clone
    newArray(index) = value
    new DoubleArray(newArray)
  }
  
  /** Returns a copy of this array with a new `value` inserted at `index`. */
  def insert(index: Int, value: Double): DoubleArray = {
    val newArray = new scala.Array[Double](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    newArray(index) = value
    java.lang.System.arraycopy(array, index, newArray, index + 1, array.length - index)
    new DoubleArray(newArray)
  }
  
  /** Returns a copy of this array with `index` removed. */
  def remove(index: Int): DoubleArray = {
    val newArray = new scala.Array[Double](array.length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new DoubleArray(newArray)
  }
  
  /** Returns a copy of this array with `value` appended. */
  def :+ (value: Double): DoubleArray = {
    val newArray = new scala.Array[Double](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length)
    newArray(newArray.length) = value
    new DoubleArray(newArray)
  }
  
  /** Returns a copy of this array with `value` prepended. */
  def +: (value: Double): DoubleArray = {
    val newArray = new scala.Array[Double](array.length + 1)
    newArray(0) = value
    java.lang.System.arraycopy(array, 0, newArray, 1, array.length)
    new DoubleArray(newArray)
  }
}

object DoubleArray {
  val empty: DoubleArray = new DoubleArray(new scala.Array[Double](0))
  
  def apply(xs: Double*): DoubleArray = macro ArrayMacros.literalDoubleArray
  
  final class Buffer extends basis.Buffer[Any, Double] {
    override type State = DoubleArray
    
    private[this] var array: scala.Array[Double] = DoubleArray.empty.array
    
    private[this] var aliased: Boolean = true
    
    private[this] var length: Int = 0
    
    private[this] def expand(base: Int, size: Int): Int = {
      var n = (base max size) - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n + 1
    }
    
    private[this] def resize(size: Int) {
      val newArray = new scala.Array[Double](size)
      java.lang.System.arraycopy(array, 0, newArray, 0, array.length min size)
      array = newArray
    }
    
    private[this] def prepare(size: Int) {
      if (aliased || size > array.length) {
        resize(expand(16, size))
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
        resize(length + count)
        aliased = false
      }
      this
    }
    
    override def state: DoubleArray = {
      if (length != array.length) resize(length)
      aliased = true
      new DoubleArray(array)
    }
    
    override def clear() {
      array = DoubleArray.empty.array
      aliased = true
      length = 0
    }
  }
}
