/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class RefArray[A](val array: Array[AnyRef]) extends AnyVal with AnyArray[A] {
  @inline override def length: Int = array.length
  
  @inline override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  @inline override def update(index: Int, value: A): Unit = array(index) = value.asInstanceOf[AnyRef]
  
  override def copy(length: Int = this.length): RefArray[A] = {
    val newArray = new Array[AnyRef](length)
    Array.copy(array, 0, newArray, 0, math.min(array.length, length))
    new RefArray[A](newArray)
  }
}

object RefArray {
  private[this] val empty = RefArray[Nothing](0)
  
  def Empty[T]: RefArray[T] = empty.asInstanceOf[RefArray[T]]
  
  @inline def apply[A](length: Int): RefArray[A] = new RefArray[A](new Array[AnyRef](length))
  
  final class Builder[A] extends basis.collection.Builder[Any, A] {
    override type Result = RefArray[A]
    
    private[this] var array: RefArray[A] = RefArray.Empty[A]
    
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
    
    override def += (value: A) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: RefArray[A] = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = RefArray.Empty[A]
      aliased = true
      length = 0
    }
  }
}
