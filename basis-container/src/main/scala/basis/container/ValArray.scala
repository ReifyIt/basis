/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.data._
import basis.util._

final class ValArray[A](mem: Mem)(implicit A: ValType[A]) extends Array[A] {
  if (mem.size % A.size.toLong != 0L)
    throw new java.lang.IllegalArgumentException("memory size not a multiple of struct size")
  
  override def isEmpty: Boolean = length == 0
  
  override val length: Int = (mem.size / A.size.toLong).toInt
  
  override def apply(index: Int): A =
    A.load(mem, A.size.toLong * index.toLong)
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update(index: Int, value: A): ValArray[A] = {
    val newMem = mem.copy()
    A.store(mem, A.size.toLong * index.toLong, value)
    new ValArray[A](newMem)
  }
  
  /** Returns a copy of this array with a new `value` inserted at `index`. */
  def insert(index: Int, value: A): ValArray[A] = {
    val offset = A.size.toLong * index.toLong
    val newMem = Mem(mem.size + A.size.toLong)
    Mem.copy(mem, 0L, newMem, 0L, offset)
    A.store(mem, offset, value)
    Mem.copy(mem, offset, newMem, offset + A.size.toLong, mem.size - offset)
    new ValArray[A](newMem)
  }
  
  /** Returns a copy of this array with `index` removed. */
  def remove(index: Int): ValArray[A] = {
    val offset = A.size.toLong * index.toLong
    val newMem = Mem(mem.size - A.size.toLong)
    Mem.copy(mem, 0L, newMem, 0L, offset)
    Mem.copy(mem, offset + A.size.toLong, newMem, offset, newMem.size - offset)
    new ValArray[A](newMem)
  }
  
  /** Returns a copy of this array with `value` appended. */
  def :+ (value: A): ValArray[A] = {
    val newMem = Mem(mem.size + A.size.toLong)
    Mem.copy(mem, 0L, newMem, 0L, mem.size)
    A.store(mem, mem.size, value)
    new ValArray[A](newMem)
  }
  
  /** Returns a copy of this array with `value` prepended. */
  def +: (value: A): ValArray[A] = {
    val newMem = Mem(mem.size + A.size.toLong)
    A.store(mem, 0L, value)
    Mem.copy(mem, 0L, newMem, A.size.toLong, mem.size)
    new ValArray[A](newMem)
  }
}

object ValArray {
  def empty[A : ValType]: ValArray[A] = new ValArray[A](Mem.alloc[A](0))
  
  final class Buffer[A](implicit A: ValType[A]) extends basis.Buffer[Any, A] {
    override type State = ValArray[A]
    
    private[this] var mem: Mem = Mem.alloc[A](0)
    
    private[this] var aliased: Boolean = false
    
    private[this] var length: Int = 0
    
    private[this] def capacity: Int = (mem.size / A.size.toLong).toInt
    
    private[this] def expand(base: Int, size: Int): Int = {
      var n = (base max size) - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n + 1
    }
    
    private[this] def resize(size: Int) {
      mem = mem.copy(A.size.toLong * size.toLong)
    }
    
    private[this] def prepare(size: Int) {
      if (aliased || size > capacity) {
        resize(expand(16, size))
        aliased = false
      }
    }
    
    override def += (value: A): this.type = {
      prepare(length + 1)
      A.store(mem, A.size.toLong * length.toLong, value)
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
      new ValArray[A](mem)
    }
    
    override def clear() {
      mem = Mem.alloc[A](0)
      aliased = false
      length = 0
    }
  }
}
