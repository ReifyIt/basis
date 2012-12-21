/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.reflect.ClassTag

/** A contiguous array.
  * 
  * @groupprio  Quantifying   -6
  * @groupprio  Indexing      -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Converting    -2
  * @groupprio  Classifying   -1
  */
abstract class ArraySeq[+A]
  extends Equals
    with Immutable
    with Family[ArraySeq[A]]
    with Index[A]
    with ArrayLike[A] {
  
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
