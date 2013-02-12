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

/** A mutable contiguous array.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Containers
  * 
  * @groupprio  Measuring     1
  * @groupprio  Indexing      2
  * @groupprio  Inserting     3
  * @groupprio  Removing      4
  * @groupprio  Traversing    5
  * @groupprio  Exporting     6
  * @groupprio  Classifying   7
  * 
  * @define collection  array buffer
  */
abstract class ArrayBuffer[A]
  extends Equals
    with Mutable
    with Family[ArrayBuffer[_]]
    with ArrayLike[A]
    with Index[A]
    with Buffer[A] {
  
  /** Returns this $collection converted to an array sequence.
    * @group Exporting */
  def toArraySeq: ArraySeq[A]
  
  protected override def stringPrefix: String = "ArrayBuffer"
}

/** A factory for specialized [[ArrayBuffer array buffers]].
  * @group Containers */
object ArrayBuffer extends SeqFactory[ArrayBuffer] {
  implicit override def Builder[A](implicit A: TypeHint[A])
    : Builder[A] { type Scope = ArrayBuffer[_]; type State = ArrayBuffer[A] } = (A match {
    case TypeHint.Byte    => new ByteArrayBufferBuilder
    case TypeHint.Short   => new ShortArrayBufferBuilder
    case TypeHint.Int     => new IntArrayBufferBuilder
    case TypeHint.Long    => new LongArrayBufferBuilder
    case TypeHint.Float   => new FloatArrayBufferBuilder
    case TypeHint.Double  => new DoubleArrayBufferBuilder
    case _                => new RefArrayBufferBuilder[A]
  }).asInstanceOf[Builder[A] { type Scope = ArrayBuffer[_]; type State = ArrayBuffer[A] }]
  
  override def toString: String = "ArrayBuffer"
}
