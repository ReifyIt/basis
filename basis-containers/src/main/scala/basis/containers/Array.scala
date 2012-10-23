/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.data._
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
    new Array.Cursor(this, 0, length)
  
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
  def Builder[A](implicit typeA: MemType[A]): Buffer[Any, A] { type State = Array[A] } = {
    import ValType._
    (typeA match {
      case typeA: RefType[A]           => new RefArray.Builder[A]
      case PackedByte                  => new ByteArray.Builder
      case PackedShort  | PaddedShort  => new ShortArray.Builder
      case PackedInt    | PaddedInt    => new IntArray.Builder
      case PackedLong   | PaddedLong   => new LongArray.Builder
      case PackedFloat  | PaddedFloat  => new FloatArray.Builder
      case PackedDouble | PaddedDouble => new DoubleArray.Builder
      case PackedBoolean               => new BitArray.Builder
      case typeA: ValType[A]           => new ValArray.Builder[A]()(typeA)
    }).asInstanceOf[Buffer[Any, A] { type State = Array[A] }]
  }
  
  private[basis] final class Cursor[+A](xs: Array[A], from: Int, until: Int) extends Iterator[A] {
    private[this] var lower: Int = 0 max from
    private[this] var upper: Int = (lower max until) min xs.length
    private[this] var index: Int = lower
    
    override def isEmpty: Boolean = index >= upper
    
    override def head: A = if (isEmpty) Done.head else xs(index)
    
    override def step(): Unit = if (isEmpty) Done.step() else index += 1
    
    override def dup: Cursor[A] = new Cursor[A](xs, index, upper)
  }
}

private[basis] class AllArrayBuffers extends ValArrayBuffers {
  implicit def ByteBuffer: ByteArray.Builder = new ByteArray.Builder
  implicit def ShortBuffer: ShortArray.Builder = new ShortArray.Builder
  implicit def IntBuffer: IntArray.Builder = new IntArray.Builder
  implicit def LongBuffer: LongArray.Builder = new LongArray.Builder
  implicit def FloatBuffer: FloatArray.Builder = new FloatArray.Builder
  implicit def DoubleBuffer: DoubleArray.Builder = new DoubleArray.Builder
  implicit def BitBuffer: BitArray.Builder = new BitArray.Builder
}

private[basis] class ValArrayBuffers extends RefArrayBuffers {
  implicit def ValBuffer[A : ValType]: ValArray.Builder[A] = new ValArray.Builder[A]
}

private[basis] class RefArrayBuffers {
  implicit def RefBuffer[A]: RefArray.Builder[A] = new RefArray.Builder[A]
}
