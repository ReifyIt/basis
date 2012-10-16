/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.data._
import basis.collection._

/** An indexed sequence of elements.
  * 
  * @author Chris Sachs
  * 
  * @define collection array
  */
trait Array[+A] extends Any with Seq[A] {
  override type Self <: Array[A]
  
  /** Returns the number of elements in this $collection. */
  def length: Int
  
  /** Returns an indexed element of this $collection. */
  def apply(index: Int): A
  
  override def iterator: Iterator[A] =
    new ArrayIterator(this, 0, length)
  
  override protected def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
}

object Array extends AllArrayBuffers with SeqFactory[Array] {
  import scala.language.experimental.macros
  import scala.language.implicitConversions
  
  implicit def Ops[A](self: Array[A]): ArrayOps[self.Self, A] =
    new ArrayOps[self.Self, A](self)
  
  def Buffer[A](implicit typeA: MemType[A]): Buffer[Any, A] { type State = Array[A] } = {
    import ValType._
    (typeA match {
      case typeA: RefType[A]           => new RefArrayBuffer[A]
      case PackedByte                  => new ByteArrayBuffer
      case PackedShort  | PaddedShort  => new ShortArrayBuffer
      case PackedInt    | PaddedInt    => new IntArrayBuffer
      case PackedLong   | PaddedLong   => new LongArrayBuffer
      case PackedFloat  | PaddedFloat  => new FloatArrayBuffer
      case PackedDouble | PaddedDouble => new DoubleArrayBuffer
      case PackedBoolean               => new BitArrayBuffer
      case typeA: ValType[A]           => new ValArrayBuffer[A]()(typeA)
    }).asInstanceOf[Buffer[Any, A] { type State = Array[A] }]
  }
  
  override protected def stringPrefix: String = "Array"
}

private[basis] class AllArrayBuffers extends ValArrayBuffers {
  implicit def ByteBuffer: ByteArrayBuffer = new ByteArrayBuffer
  implicit def ShortBuffer: ShortArrayBuffer = new ShortArrayBuffer
  implicit def IntBuffer: IntArrayBuffer = new IntArrayBuffer
  implicit def LongBuffer: LongArrayBuffer = new LongArrayBuffer
  implicit def FloatBuffer: FloatArrayBuffer = new FloatArrayBuffer
  implicit def DoubleBuffer: DoubleArrayBuffer = new DoubleArrayBuffer
  implicit def BitBuffer: BitArrayBuffer = new BitArrayBuffer
}

private[basis] class ValArrayBuffers extends RefArrayBuffers {
  implicit def ValBuffer[A](implicit typeA: ValType[A]): ValArrayBuffer[A] = new ValArrayBuffer[A]
}

private[basis] class RefArrayBuffers {
  implicit def RefBuffer[A]: RefArrayBuffer[A] = new RefArrayBuffer[A]
}

private[basis] final class ArrayIterator[+A]
    (xs: Array[A], from: Int, until: Int)
  extends Iterator[A] {
  
  import java.lang.Math.{max, min}
  
  private[this] var lower: Int = 0 max from
  private[this] var upper: Int = (lower max until) min xs.length
  private[this] var index: Int = lower
  
  override def isEmpty: Boolean = index >= upper
  
  override def head: A = {
    if (isEmpty) throw new scala.NoSuchElementException("head of empty iterator")
    else xs(index)
  }
  
  override def step() {
    if (isEmpty) throw new java.lang.UnsupportedOperationException("empty iterator step")
    else index += 1
  }
  
  override def dup: ArrayIterator[A] =
    new ArrayIterator[A](xs, index, upper)
}
