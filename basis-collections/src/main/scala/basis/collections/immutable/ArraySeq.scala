//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis.collections.generic._
import basis.collections.mutable._
import scala.reflect._

abstract class ArraySeq[+A]
  extends Equals
  with Immutable
  with Family[ArraySeq[_]]
  with ArrayLike[A]
  with IndexedSeq[A] {

  /** Returns a copy of this $collection with the given element at the given index.
    * @group Indexing */
  def update[B >: A](index: Int, elem: B): ArraySeq[B] = {
    val n = length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[AnyRef](n)
    copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, index)
    newArray(index) = elem.asInstanceOf[AnyRef]
    val i = index + 1
    copyToArray(i, newArray.asInstanceOf[Array[Any]], i, n - i)
    new RefArraySeq(newArray)
  }

  /** Returns a copy of this $collection with the given element appended.
    * @group Inserting */
  def append[B >: A](elem: B): ArraySeq[B] = {
    val n = length
    val newArray = new Array[AnyRef](n + 1)
    copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, n)
    newArray(n) = elem.asInstanceOf[AnyRef]
    new RefArraySeq(newArray)
  }

  /** Returns a copy of this $collection with the given elements appended.
    * @group Inserting */
  def appendAll[B >: A](elems: Traverser[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[B]]
      val m = length
      val n = xs.length
      val newArray = new Array[AnyRef](m + n)
      copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, m)
      xs.copyToArray(0, newArray.asInstanceOf[Array[Any]], m, n)
      new RefArraySeq(newArray)
    }
    else appendAll(ArrayBuffer.coerce(elems))
  }

  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def prepend[B >: A](elem: B): ArraySeq[B] = {
    val n = length
    val newArray = new Array[AnyRef](n + 1)
    newArray(0) = elem.asInstanceOf[AnyRef]
    copyToArray(0, newArray.asInstanceOf[Array[Any]], 1, n)
    new RefArraySeq(newArray)
  }

  /** Returns a copy of this $collection with the given elements prepended.
    * @group Inserting */
  def prependAll[B >: A](elems: Traverser[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[B]]
      val m = xs.length
      val n = length
      val newArray = new Array[AnyRef](m + n)
      xs.copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, m)
      copyToArray(0, newArray.asInstanceOf[Array[Any]], m, n)
      new RefArraySeq(newArray)
    }
    else prependAll(ArrayBuffer.coerce(elems))
  }

  /** Returns a copy of this $collection with the given element inserted at the given index.
    * @group Inserting */
  def insert[B >: A](index: Int, elem: B): ArraySeq[B] = {
    val n = length
    if (index < 0 || index > n) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[AnyRef](n + 1)
    copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, index)
    newArray(index) = elem.asInstanceOf[AnyRef]
    copyToArray(index, newArray.asInstanceOf[Array[Any]], index + 1, n - index)
    new RefArraySeq(newArray)
  }

  /** Returns a copy of this $collection with the given elements inserted at the given index.
    * @group Inserting */
  def insertAll[B >: A](index: Int, elems: Traverser[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[B]]
      val m = length
      val n = xs.length
      val newArray = new Array[AnyRef](m + n)
      copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, index)
      xs.copyToArray(0, newArray.asInstanceOf[Array[Any]], index, n)
      copyToArray(index, newArray.asInstanceOf[Array[Any]], index + n, m - index)
      new RefArraySeq(newArray)
    }
    else insertAll(index, ArrayBuffer.coerce(elems))
  }

  /** Returns a copy of this $collection with the given index removed.
    * @group Removing */
  def remove(index: Int): ArraySeq[A] = {
    val n = length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[AnyRef](n - 1)
    copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, index)
    val i = index + 1
    copyToArray(i, newArray.asInstanceOf[Array[Any]], index, n - i)
    new RefArraySeq(newArray)
  }

  /** Returns a copy of this $collection with the given index range removed.
    * @group Removing */
  def remove(index: Int, count: Int): ArraySeq[A] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    val n = length
    val i = index + count
    if (i > n) throw new IndexOutOfBoundsException((i - 1).toString)
    val newArray = new Array[AnyRef](n - count)
    copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, index)
    copyToArray(i, newArray.asInstanceOf[Array[Any]], index, n - i)
    new RefArraySeq(newArray)
  }

  /** Returns a copy of this $collection with the given element appended.
    * @group Inserting */
  def :+ [B >: A](elem: B): ArraySeq[B] = append(elem)

  /** Returns a copy of this $collection with the given element appended.
    * @group Inserting */
  def :++ [B >: A](elems: Traverser[B]): ArraySeq[B] = appendAll(elems)

  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def +: [B >: A](elem: B): ArraySeq[B] = prepend(elem)

  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def ++: [B >: A](elems: Traverser[B]): ArraySeq[B] = prependAll(elems)

  protected override def stringPrefix: String = "ArraySeq"
}

object ArraySeq extends ArrayFactory[ArraySeq] {
  implicit override def Builder[A]()(implicit A: ClassTag[A])
    : ArrayBuilder[A] with State[ArraySeq[A]] = (A match {
    case ClassTag.Byte    => new ByteArraySeqBuilder
    case ClassTag.Short   => new ShortArraySeqBuilder
    case ClassTag.Int     => new IntArraySeqBuilder
    case ClassTag.Long    => new LongArraySeqBuilder
    case ClassTag.Float   => new FloatArraySeqBuilder
    case ClassTag.Double  => new DoubleArraySeqBuilder
    case ClassTag.Boolean => new BitArraySeqBuilder
    case _                => new RefArraySeqBuilder[A]
  }).asInstanceOf[ArrayBuilder[A] with State[ArraySeq[A]]]

  override def from[A](elems: Traverser[A])(implicit A: ClassTag[A]): ArraySeq[A] = {
    if (elems.isInstanceOf[ArraySeq[_]]) elems.asInstanceOf[ArraySeq[A]]
    else super.from(elems)
  }

  override def toString: String = "ArraySeq"
}
