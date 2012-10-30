/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.memory._
import basis.util._

final class ValArray[A](data: Data)(implicit A: ValType[A]) extends Array[A] {
  if (data.size % A.size.toLong != 0L)
    throw new java.lang.IllegalArgumentException("Data size not a multiple of struct size.")
  
  override def isEmpty: Boolean = length == 0
  
  override val length: Int = (data.size / A.size.toLong).toInt
  
  override def apply(index: Int): A =
    A.load(data, A.size.toLong * index.toLong)
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update(index: Int, value: A): ValArray[A] = {
    val newData = data.copy()
    A.store(data, A.size.toLong * index.toLong, value)
    new ValArray[A](newData)
  }
  
  /** Returns a copy of this array with a new `value` inserted at `index`. */
  def insert(index: Int, value: A): ValArray[A] = {
    val offset = A.size.toLong * index.toLong
    val newData = Data(data.size + A.size.toLong)
    Data.copy(data, 0L, newData, 0L, offset)
    A.store(data, offset, value)
    Data.copy(data, offset, newData, offset + A.size.toLong, data.size - offset)
    new ValArray[A](newData)
  }
  
  /** Returns a copy of this array with `index` removed. */
  def remove(index: Int): ValArray[A] = {
    val offset = A.size.toLong * index.toLong
    val newData = Data(data.size - A.size.toLong)
    Data.copy(data, 0L, newData, 0L, offset)
    Data.copy(data, offset + A.size.toLong, newData, offset, newData.size - offset)
    new ValArray[A](newData)
  }
  
  /** Returns a copy of this array with `value` appended. */
  def :+ (value: A): ValArray[A] = {
    val newData = Data(data.size + A.size.toLong)
    Data.copy(data, 0L, newData, 0L, data.size)
    A.store(data, data.size, value)
    new ValArray[A](newData)
  }
  
  /** Returns a copy of this array with `value` prepended. */
  def +: (value: A): ValArray[A] = {
    val newData = Data(data.size + A.size.toLong)
    A.store(data, 0L, value)
    Data.copy(data, 0L, newData, A.size.toLong, data.size)
    new ValArray[A](newData)
  }
  
  protected override def stringPrefix: String = "ValArray"
}

object ValArray {
  def Empty[A : ValType]: ValArray[A] = new ValArray[A](Data.alloc[A](0))
  
  final class Builder[A](implicit A: ValType[A]) extends Buffer[Any, A] {
    override type State = ValArray[A]
    
    private[this] var data: Data = Data.alloc[A](0)
    
    private[this] var aliased: Boolean = false
    
    private[this] var length: Int = 0
    
    private[this] def capacity: Int = (data.size / A.size.toLong).toInt
    
    private[this] def expand(base: Int, size: Int): Int = {
      var n = (base max size) - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n + 1
    }
    
    private[this] def resize(size: Int) {
      data = data.copy(A.size.toLong * size.toLong)
    }
    
    private[this] def prepare(size: Int) {
      if (aliased || size > capacity) {
        resize(expand(16, size))
        aliased = false
      }
    }
    
    override def += (value: A): this.type = {
      prepare(length + 1)
      A.store(data, A.size.toLong * length.toLong, value)
      length += 1
      this
    }
    
    override def expect(count: Int): this.type = {
      if (length + count > capacity) {
        resize(length + count)
        aliased = false
      }
      this
    }
    
    override def state: ValArray[A] = {
      if (length != capacity) resize(length)
      aliased = true
      new ValArray[A](data)
    }
    
    override def clear() {
      data = Data.alloc[A](0)
      aliased = false
      length = 0
    }
  }
}
