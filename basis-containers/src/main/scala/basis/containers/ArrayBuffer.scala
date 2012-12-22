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

/** A mutable contiguous array.
  * 
  * @groupprio  Quantifying   -8
  * @groupprio  Indexing      -7
  * @groupprio  Inserting     -6
  * @groupprio  Removing      -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Converting    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  array buffer
  */
abstract class ArrayBuffer[A]
  extends Equals
    with Mutable
    with Family[ArrayBuffer[A]]
    with Index[A]
    with Buffer[A]
    with ArrayLike[A] {
  
  /** Returns this $collection converted to an array sequence.
    * @group Converting */
  def toArraySeq: ArraySeq[A]
  
  protected override def stringPrefix: String = "ArrayBuffer"
}

object ArrayBuffer extends SeqFactory[ArrayBuffer] {
  implicit override def Builder[A](implicit A: ClassTag[A])
    : Builder[Any, A] { type State = ArrayBuffer[A] } = (A match {
    case ClassTag.Byte    => new ByteArrayBufferBuilder
    case ClassTag.Short   => new ShortArrayBufferBuilder
    case ClassTag.Int     => new IntArrayBufferBuilder
    case ClassTag.Long    => new LongArrayBufferBuilder
    case ClassTag.Float   => new FloatArrayBufferBuilder
    case ClassTag.Double  => new DoubleArrayBufferBuilder
    case _                => new RefArrayBufferBuilder[A]
  }).asInstanceOf[Builder[Any, A] { type State = ArrayBuffer[A] }]
  
  override def toString: String = "ArrayBuffer"
}