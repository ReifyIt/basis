//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package mutable

import basis.collections.generic._
import basis.collections.immutable._
import scala.reflect._

abstract class ArrayBuffer[A]
  extends Equals
  with Mutable
  with Family[ArrayBuffer[_]]
  with ArrayLike[A]
  with ArrayBuilder[A]
  with Buffer[A]
  with IndexedSeq[A] {

  /** Prepends an array of elements to this $collection.
    * @group Inserting */
  def prependArray(elems: Array[A]): Unit = {
    var i = 0
    val n = length
    while (i < n) {
      insert(i, elems(i))
      i += 1
    }
  }

  /** Inserts an array of elements into this $collection, starting at `index`.
    * @group Inserting */
  def insertArray(index: Int, elems: Array[A]): Unit = {
    var i = 0
    val n = length
    while (i < n) {
      insert(index + i, elems(i))
      i += 1
    }
  }

  override def ++= (elems: Traverser[A]): this.type = {
    appendAll(elems)
    this
  }

  override def ++= (elems: Array[A]): this.type = {
    appendArray(elems)
    this
  }

  override def ++=: (elems: Traverser[A]): this.type = {
    prependAll(elems)
    this
  }

  /** Prepends an array of elements to this $collection.
    * @group Inserting */
  def ++=: (elems: Array[A]): this.type = {
    prependArray(elems)
    this
  }

  /** Returns this $collection converted to an array sequence.
    * @group Exporting */
  def toArraySeq: ArraySeq[A]

  protected override def stringPrefix: String = "ArrayBuffer"
}

object ArrayBuffer extends ArrayFactory[ArrayBuffer] {
  implicit override def Builder[A](implicit A: ClassTag[A])
    : ArrayBuilder[A] with State[ArrayBuffer[A]] = (A match {
    case ClassTag.Byte   => new ByteArrayBufferBuilder
    case ClassTag.Short  => new ShortArrayBufferBuilder
    case ClassTag.Int    => new IntArrayBufferBuilder
    case ClassTag.Long   => new LongArrayBufferBuilder
    case ClassTag.Float  => new FloatArrayBufferBuilder
    case ClassTag.Double => new DoubleArrayBufferBuilder
    case _               => new RefArrayBufferBuilder[A]
  }).asInstanceOf[ArrayBuilder[A] with State[ArrayBuffer[A]]]

  override def from[A](elems: Traverser[A])(implicit A: ClassTag[A]): ArrayBuffer[A] = {
    if (elems.isInstanceOf[ArrayBuffer[_]]) elems.asInstanceOf[ArrayBuffer[A]]
    else super.from(elems)
  }

  override def toString: String = "ArrayBuffer"
}
