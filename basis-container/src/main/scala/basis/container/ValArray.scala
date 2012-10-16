/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.data._

final class ValArray[+A](data: Mem)(implicit typeA: ValType[A]) extends Array[A] {
  import scala.annotation.unchecked.uncheckedVariance
  
  override val length: Int = (data.size / typeA.size.toLong).toInt
  
  override def apply(index: Int): A =
    typeA.load(data, typeA.size.toLong * index.toLong)
  
  private[basis] def update(index: Int, value: A @uncheckedVariance): Unit =
    typeA.store(data, typeA.size.toLong * index.toLong, value)
  
  private[basis] def copy(length: Int): ValArray[A] =
    new ValArray[A](data.copy(typeA.size.toLong * length.toLong))
}

private[basis] object ValArray {
  val empty: ValArray[Nothing] = ValArray[Byte](0).asInstanceOf[ValArray[Nothing]]
  
  def apply[A](length: Int)(implicit typeA: ValType[A]): ValArray[A] =
    new ValArray[A](Mem.alloc[A](length))
}

final class ValArrayBuffer[A](implicit typeA: ValType[A]) extends Buffer[Any, A] {
  override type State = ValArray[A]
  
  private[this] var array: ValArray[A] = ValArray.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      array = array.copy(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: A): this.type = {
    prepare(length + 1)
    array(length) = value
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ValArray[A] = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = ValArray.empty
    aliased = false
    length = 0
  }
}
