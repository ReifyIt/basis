/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._
import basis.util._

import scala.reflect.ClassTag

private[containers] final class ByteArrayBuilder extends ByteArrayBuffer {
  override type Scope = Array[_]
  override type State = Array[Byte]
  override def state: Array[Byte] = toArray
  override def toString: String = "ArrayBuilder"+"["+"Byte"+"]"
}

private[containers] final class ShortArrayBuilder extends ShortArrayBuffer {
  override type Scope = Array[_]
  override type State = Array[Short]
  override def state: Array[Short] = toArray
  override def toString: String = "ArrayBuilder"+"["+"Short"+"]"
}

private[containers] final class IntArrayBuilder extends IntArrayBuffer {
  override type Scope = Array[_]
  override type State = Array[Int]
  override def state: Array[Int] = toArray
  override def toString: String = "ArrayBuilder"+"["+"Int"+"]"
}

private[containers] final class LongArrayBuilder extends LongArrayBuffer {
  override type Scope = Array[_]
  override type State = Array[Long]
  override def state: Array[Long] = toArray
  override def toString: String = "ArrayBuilder"+"["+"Long"+"]"
}

private[containers] final class FloatArrayBuilder extends FloatArrayBuffer {
  override type Scope = Array[_]
  override type State = Array[Float]
  override def state: Array[Float] = toArray
  override def toString: String = "ArrayBuilder"+"["+"Float"+"]"
}

private[containers] final class DoubleArrayBuilder extends DoubleArrayBuffer {
  override type Scope = Array[_]
  override type State = Array[Double]
  override def state: Array[Double] = toArray
  override def toString: String = "ArrayBuilder"+"["+"Double"+"]"
}

private[containers] final class RefArrayBuilder[A](implicit A: ClassTag[A]) extends ArrayBuilder[A] {
  override type Scope = Array[_]
  override type State = Array[A]
  
  private[this] var buffer: Array[A] = _
  private[this] var size: Int = 0
  private[this] var aliased: Boolean = true
  
  protected def defaultSize: Int = 16
  
  override def append(elem: A) {
    var array = buffer
    if (aliased || size + 1 > array.length) {
      array = A.newArray(expand(size + 1))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    array(size) = elem
    size += 1
  }
  
  override def appendAll(elems: Enumerator[A]) {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[A]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = A.newArray(expand(size + n))
        if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
        buffer = array
        aliased = false
      }
      xs.copyToArray(0, array, size, n)
      size += n
    }
    else appendAll(ArrayBuffer.coerce(elems))
  }
  
  override def appendArray(elems: Array[A]) {
    val n = elems.length
    var array = buffer
    if (aliased || size + n > array.length) {
      array = A.newArray(expand(size + n))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    Array.copy(elems, 0, array, size, n)
    size += n
  }
  
  override def expect(count: Int): this.type = {
    if (buffer == null || size + count > buffer.length) {
      val array = A.newArray(size + count)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    this
  }
  
  override def state: Array[A] = {
    var array = buffer
    if (array == null || size != array.length) {
      array = A.newArray(size)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
    }
    aliased = true
    array
  }
  
  override def clear() {
    aliased = true
    size = 0
    buffer = null
  }
  
  private[this] def expand(size: Int): Int = {
    var n = (defaultSize max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  override def toString: String = "ArrayBuilder"+"["+ A +"]"
}
