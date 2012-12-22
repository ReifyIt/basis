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
  * @groupprio  Quantifying   -8
  * @groupprio  Indexing      -7
  * @groupprio  Inserting     -6
  * @groupprio  Removing      -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Converting    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  array sequence
  */
abstract class ArraySeq[+A]
  extends Equals
    with Immutable
    with Family[ArraySeq[A]]
    with Index[A]
    with ArrayLike[A] {
  
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
  def appendAll[B >: A](elems: Enumerator[B]): ArraySeq[B] = {
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
  def prependAll[B >: A](elems: Enumerator[B]): ArraySeq[B] = {
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
  def insertAll[B >: A](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
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
  def :++ [B >: A](elems: Enumerator[B]): ArraySeq[B] = appendAll(elems)
  
  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def +: [B >: A](elem: B): ArraySeq[B] = prepend(elem)
  
  /** Returns a copy of this $collection with the given element prepended.
    * @group Inserting */
  def ++: [B >: A](elems: Enumerator[B]): ArraySeq[B] = prependAll(elems)
  
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