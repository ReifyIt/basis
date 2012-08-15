/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis.memory._

final class ValArray[A](val data: Data)(implicit typeA: ValType[A]) extends AnyArray[A] {
  override def length: Int = (data.size / typeA.size.toLong).toInt
  
  override def apply(index: Int): A =
    typeA.load(data, typeA.size.toLong * index.toLong)
  
  override def update(index: Int, value: A): Unit =
    typeA.store(data, typeA.size.toLong * index.toLong, value)
  
  override def copy(length: Int = this.length): ValArray[A] =
    new ValArray[A](data.copy(typeA.size.toLong * length.toLong))
}

object ValArray {
  def apply[A](length: Int)(implicit typeA: ValType[A]): ValArray[A] =
    new ValArray[A](Data.alloc[A](length))
  
  final class Builder[A](implicit typeA: ValType[A]) extends basis.collection.Builder[Any, A] {
    override type Result = ValArray[A]
    
    private[this] var array: ValArray[A] = ValArray[A](16)
    
    private[this] var aliased: Boolean = false
    
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
    
    override def += (value: A) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: ValArray[A] = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = ValArray[A](16)
      aliased = false
      length = 0
    }
  }
}
