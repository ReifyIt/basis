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

/** A contiguous array of elements.
  * 
  * @define collection  array
  */
abstract class ArraySeq[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Family[ArraySeq[A]] with IndexedSeq[A] {
  
  override def isEmpty: Boolean
  
  override def length: Int
  
  override def apply(index: Int): A
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update[B >: A](index: Int, value: B): ArraySeq[B] = Predef.???
  
  /** Returns a copy of this array with a new `value` inserted at `index`. */
  def insert[B >: A](index: Int, value: B): ArraySeq[B] = Predef.???
  
  /** Returns a copy of this array with `index` removed. */
  def remove(index: Int): ArraySeq[A] = Predef.???
  
  /** Returns a copy of this array with `value` appended. */
  def :+ [B >: A](value: B): ArraySeq[B] = Predef.???
  
  /** Returns a copy of this array with `value` prepended. */
  def +: [B >: A](value: B): ArraySeq[B] = Predef.???
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: ArraySeq[A] =>
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
    val s = new java.lang.StringBuilder("ArraySeq")
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
}

object ArraySeq {
  def apply[A](xs: A*)(implicit A: DataType[A]): ArraySeq[A] =
    macro ArraySeqMacros.apply[A]
  
  implicit def Builder[A](implicit A: DataType[A]): Builder[Any, A, ArraySeq[A]] = {
    import ValType._
    (A match {
      case _ : RefType[A]              => new RefArraySeqBuilder[A]
      case PackedByte                  => new ByteArraySeqBuilder
      case PackedShort  | PaddedShort  => new ShortArraySeqBuilder
      case PackedInt    | PaddedInt    => new IntArraySeqBuilder
      case PackedLong   | PaddedLong   => new LongArraySeqBuilder
      case PackedFloat  | PaddedFloat  => new FloatArraySeqBuilder
      case PackedDouble | PaddedDouble => new DoubleArraySeqBuilder
      case PackedBoolean               => new BitArraySeqBuilder
      case struct: ValType[A]          => new ValArraySeqBuilder[A]()(struct)
    }).asInstanceOf[Builder[Any, A, ArraySeq[A]]]
  }
}
