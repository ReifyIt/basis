/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.annotation.unspecialized
import scala.reflect.ClassTag

/** A contiguous array.
  * 
  * @groupprio  Examining     -6
  * @groupprio  Copying       -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Converting    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  sequence
  */
abstract class ArraySeq[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Equals with Family[ArraySeq[A]] with Index[A] with ArrayLike[A] {
  
  override def length: Int
  
  override def apply(index: Int): A
  
  @unspecialized override def copyToArray[B >: A](xs: Array[B], start: Int, count: Int) {
    var i = 0
    var j = start
    val n = count min (xs.length - start) min length
    while (i < n) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }
  
  @unspecialized override def copyToArray[B >: A](xs: Array[B], start: Int) {
    var i = 0
    var j = start
    val n = (xs.length - start) min length
    while (i < n) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }
  
  @unspecialized override def copyToArray[B >: A](xs: Array[B]) {
    var i = 0
    val n = xs.length min length
    while (i < n) {
      xs(i) = this(i)
      i += 1
    }
  }
  
  @unspecialized override def toArray[B >: A](implicit B: ClassTag[B]): Array[B] = {
    var i = 0
    val n = length
    val xs = B.newArray(n)
    while (i < n) {
      xs(i) = this(i)
      i += 1
    }
    xs
  }
  
  protected override def stringPrefix: String = "ArraySeq"
}

object ArraySeq extends SeqFactory[ArraySeq] {
  implicit override def Builder[A](implicit A: ClassTag[A])
    : Builder[Any, A] { type State = ArraySeq[A] } = (A match {
    case ClassTag.Byte    => new ByteArraySeqBuilder
    case ClassTag.Short   => new ShortArraySeqBuilder
    case ClassTag.Int     => new IntArraySeqBuilder
    case ClassTag.Long    => new LongArraySeqBuilder
    case ClassTag.Float   => new FloatArraySeqBuilder
    case ClassTag.Double  => new DoubleArraySeqBuilder
    case ClassTag.Boolean => new BitArraySeqBuilder
    case _                => new RefArraySeqBuilder[A]
  }).asInstanceOf[Builder[Any, A] { type State = ArraySeq[A] }]
  
  override def toString: String = "ArraySeq"
}
