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
import basis.util._

/** An indexed sequence of elements.
  * 
  * @author Chris Sachs
  * 
  * @define collection  array
  */
trait Array[+A] extends Any with Seq[A] {
  override type Self <: Array[A]
  
  /** Returns the element at `index`. */
  def apply(index: Int): A
  
  override def iterator: Iterator[A] =
    new Array.Iterator(this, 0, length)
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: Array[A] =>
      var i = 0
      val n = length
      var e = n == that.length
      while (e && i < n) {
        e = this(i) == that(i)
        i += 1
      }
      e
    case _ => false
  }
  
  override def hashCode: Int = {
    import MurmurHash3._
    var h = 63537721
    var i = 0
    val n = length
    while (i < n) {
      h = mix(h, this(i).##)
      i += 1
    }
    mash(h)
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix)
    s.append('(')
    if (!isEmpty) {
      s.append(this(0))
      var i = 1
      val n = length
      while (i < length) {
        s.append(", ").append(this(i))
        i += 1
      }
    }
    s.append(')')
    s.toString
  }
  
  protected def stringPrefix: String = "Array"
}

object Array extends AllArrayBuffers with SeqFactory[Array] {
  def Buffer[A](implicit typeA: MemType[A]): Buffer[Any, A] { type State = Array[A] } = {
    import ValType._
    (typeA match {
      case typeA: RefType[A]           => new RefArray.Buffer[A]
      case PackedByte                  => new ByteArray.Buffer
      case PackedShort  | PaddedShort  => new ShortArray.Buffer
      case PackedInt    | PaddedInt    => new IntArray.Buffer
      case PackedLong   | PaddedLong   => new LongArray.Buffer
      case PackedFloat  | PaddedFloat  => new FloatArray.Buffer
      case PackedDouble | PaddedDouble => new DoubleArray.Buffer
      case PackedBoolean               => new BitArray.Buffer
      case typeA: ValType[A]           => new ValArray.Buffer[A]()(typeA)
    }).asInstanceOf[Buffer[Any, A] { type State = Array[A] }]
  }
  
  private[basis] final class Iterator[+A]
      (xs: Array[A], from: Int, until: Int)
    extends basis.Iterator[A] {
    
    private[this] var lower: Int = 0 max from
    private[this] var upper: Int = (lower max until) min xs.length
    private[this] var index: Int = lower
    
    override def isEmpty: Boolean = index >= upper
    
    override def head: A = {
      if (isEmpty) Iterator.empty.head
      else xs(index)
    }
    
    override def step() {
      if (isEmpty) Iterator.empty.step
      else index += 1
    }
    
    override def dup: Array.Iterator[A] =
      new Array.Iterator[A](xs, index, upper)
  }
}

private[basis] class AllArrayBuffers extends ValArrayBuffers {
  implicit def ByteBuffer: ByteArray.Buffer = new ByteArray.Buffer
  implicit def ShortBuffer: ShortArray.Buffer = new ShortArray.Buffer
  implicit def IntBuffer: IntArray.Buffer = new IntArray.Buffer
  implicit def LongBuffer: LongArray.Buffer = new LongArray.Buffer
  implicit def FloatBuffer: FloatArray.Buffer = new FloatArray.Buffer
  implicit def DoubleBuffer: DoubleArray.Buffer = new DoubleArray.Buffer
  implicit def BitBuffer: BitArray.Buffer = new BitArray.Buffer
}

private[basis] class ValArrayBuffers extends RefArrayBuffers {
  implicit def ValBuffer[A : ValType]: ValArray.Buffer[A] = new ValArray.Buffer[A]
}

private[basis] class RefArrayBuffers {
  implicit def RefBuffer[A]: RefArray.Buffer[A] = new RefArray.Buffer[A]
}
