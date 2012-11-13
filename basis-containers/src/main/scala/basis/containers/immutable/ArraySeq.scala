/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.collections.general._
import basis.memory._
import basis.util._

/** A contiguous immutable array.
  * 
  * @define collection  array
  */
abstract class ArraySeq[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Equals
    with Immutable
    with Family[ArraySeq[A]]
    with immutable.IndexedSeq[A] {
  
  override def isEmpty: Boolean
  
  override def length: Int
  
  override def apply(index: Int): A
  
  override def update[B >: A](index: Int, value: B): ArraySeq[B] = Predef.???
  
  /** Returns a copy of this array with a new `value` inserted at `index`.
    * @group Updating */
  def insert[B >: A](index: Int, value: B): ArraySeq[B] = Predef.???
  
  /** Returns a copy of this array with `index` removed.
    * @group Updating */
  def remove(index: Int): ArraySeq[A] = Predef.???
  
  /** Returns a copy of this array with `value` appended. */
  override def :+ [B >: A](value: B): ArraySeq[B] = Predef.???
  
  /** Returns a copy of this array with `value` prepended. */
  override def +: [B >: A](value: B): ArraySeq[B] = Predef.???
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
  
  protected override def stringPrefix: String = "ArraySeq"
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
