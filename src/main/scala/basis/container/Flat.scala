/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis.collection._
import basis.memory._

trait Flat[A] extends Any with Indexing[Flat[_], A] {
  override def length: Int
  
  override def apply(index: Int): A
  
  def update(index: Int, value: A): Unit
  
  def copy(length: Int = this.length): Flat[A]
}

object Flat extends FlatBuilders.ArrayBuilders {
  import DataType._
  
  def apply[A](length: Int)(implicit typeA: DataType[A]): Flat[A] = typeA match {
    case typeA: RefType[A]           => RefArray[A](length)
    case PackedByte                  => ByteArray(length).asInstanceOf[Flat[A]]
    case PackedShort  | PaddedShort  => ShortArray(length).asInstanceOf[Flat[A]]
    case PackedInt    | PaddedInt    => IntArray(length).asInstanceOf[Flat[A]]
    case PackedLong   | PaddedLong   => LongArray(length).asInstanceOf[Flat[A]]
    case PackedFloat  | PaddedFloat  => FloatArray(length).asInstanceOf[Flat[A]]
    case PackedDouble | PaddedDouble => DoubleArray(length).asInstanceOf[Flat[A]]
    case PackedChar   | PaddedChar   => CharArray(length).asInstanceOf[Flat[A]]
    case PackedBoolean               => BitArray(length).asInstanceOf[Flat[A]]
    case typeA: ValType[A]           => ValArray[A](length)(typeA)
  }
  
  def Builder[A](implicit typeA: DataType[A]): Collector[Any, A] = typeA match {
    case typeA: RefType[A]           => RefArrayBuilder[A]
    case PackedByte                  => ByteArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedShort  | PaddedShort  => ShortArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedInt    | PaddedInt    => IntArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedLong   | PaddedLong   => LongArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedFloat  | PaddedFloat  => FloatArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedDouble | PaddedDouble => DoubleArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedChar   | PaddedChar   => CharArrayBuilder.asInstanceOf[Collector[Any, A]]
    case PackedBoolean               => BitArrayBuilder.asInstanceOf[Collector[Any, A]]
    case typeA: ValType[A]           => ValArrayBuilder[A](typeA)
  }
  
  object ValArray {
    def apply[A](length: Int)(implicit typeA: ValType[A]): ValArray[A] = new ValArray[A](Data.alloc[A](length))
  }
  
  object RefArray {
    private[this] val empty = RefArray[Nothing](0)
    def Empty[T]: RefArray[T] = empty.asInstanceOf[RefArray[T]]
    @inline def apply[A](length: Int): RefArray[A] = new RefArray[A](new Array[AnyRef](length))
  }
  
  object ByteArray {
    val Empty = ByteArray(0)
    @inline def apply(length: Int): ByteArray = new ByteArray(new Array[Byte](length))
  }
  
  object ShortArray {
    val Empty = ShortArray(0)
    @inline def apply(length: Int): ShortArray = new ShortArray(new Array[Short](length))
  }
  
  object IntArray {
    val Empty = IntArray(0)
    @inline def apply(length: Int): IntArray = new IntArray(new Array[Int](length))
  }
  
  object LongArray {
    val Empty = LongArray(0)
    @inline def apply(length: Int): LongArray = new LongArray(new Array[Long](length))
  }
  
  object FloatArray {
    val Empty = FloatArray(0)
    @inline def apply(length: Int): FloatArray = new FloatArray(new Array[Float](length))
  }
  
  object DoubleArray {
    val Empty = DoubleArray(0)
    @inline def apply(length: Int): DoubleArray = new DoubleArray(new Array[Double](length))
  }
  
  object CharArray {
    val Empty = CharArray(0)
    @inline def apply(length: Int): CharArray = new CharArray(new Array[Char](length))
  }
  
  object BitArray {
    val Empty = BitArray(0)
    def apply(length: Int): BitArray = {
      val words = new Array[Int](1 + (((length + 31) & ~31) >> 5))
      words(0) = length
      new BitArray(words)
    }
  }
  
  final class ValArray[A](val data: Data)(implicit typeA: ValType[A]) extends Flat[A] {
    override def length: Int = (data.size / typeA.size.toLong).toInt
    override def apply(index: Int): A = typeA.load(data, typeA.size.toLong * index.toLong)
    override def update(index: Int, value: A): Unit = typeA.store(data, typeA.size.toLong * index.toLong, value)
    override def copy(length: Int): ValArray[A] = new ValArray[A](data.copy(typeA.size.toLong * length.toLong))
  }
  
  final class RefArray[A](val array: Array[AnyRef]) extends AnyVal with Flat[A] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): A = array(index).asInstanceOf[A]
    @inline override def update(index: Int, value: A): Unit = array(index) = value.asInstanceOf[AnyRef]
    override def copy(length: Int): RefArray[A] = {
      val newArray = new Array[AnyRef](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new RefArray[A](newArray)
    }
  }
  
  final class ByteArray(val array: Array[Byte]) extends AnyVal with Flat[Byte] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Byte = array(index)
    @inline override def update(index: Int, value: Byte): Unit = array(index) = value
    override def copy(length: Int): ByteArray = {
      val newArray = new Array[Byte](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new ByteArray(newArray)
    }
  }
  
  final class ShortArray(val array: Array[Short]) extends AnyVal with Flat[Short] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Short = array(index)
    @inline override def update(index: Int, value: Short): Unit = array(index) = value
    override def copy(length: Int): ShortArray = {
      val newArray = new Array[Short](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new ShortArray(newArray)
    }
  }
  
  final class IntArray(val array: Array[Int]) extends AnyVal with Flat[Int] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Int = array(index)
    @inline override def update(index: Int, value: Int): Unit = array(index) = value
    override def copy(length: Int): IntArray = {
      val newArray = new Array[Int](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new IntArray(newArray)
    }
  }
  
  final class LongArray(val array: Array[Long]) extends AnyVal with Flat[Long] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Long = array(index)
    @inline override def update(index: Int, value: Long): Unit = array(index) = value
    override def copy(length: Int): LongArray = {
      val newArray = new Array[Long](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new LongArray(newArray)
    }
  }
  
  final class FloatArray(val array: Array[Float]) extends AnyVal with Flat[Float] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Float = array(index)
    @inline override def update(index: Int, value: Float): Unit = array(index) = value
    override def copy(length: Int): FloatArray = {
      val newArray = new Array[Float](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new FloatArray(newArray)
    }
  }
  
  final class DoubleArray(val array: Array[Double]) extends AnyVal with Flat[Double] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Double = array(index)
    @inline override def update(index: Int, value: Double): Unit = array(index) = value
    override def copy(length: Int): DoubleArray = {
      val newArray = new Array[Double](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new DoubleArray(newArray)
    }
  }
  
  final class CharArray(val array: Array[Char]) extends AnyVal with Flat[Char] {
    @inline override def length: Int = array.length
    @inline override def apply(index: Int): Char = array(index)
    @inline override def update(index: Int, value: Char): Unit = array(index) = value
    override def copy(length: Int): CharArray = {
      val newArray = new Array[Char](length)
      Array.copy(array, 0, newArray, 0, math.min(array.length, length))
      new CharArray(newArray)
    }
  }
  
  final class BitArray(val words: Array[Int]) extends AnyVal with Flat[Boolean] {
    override def length: Int = words(0)
    override def apply(index: Int): Boolean = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      ((words(1 + (index >> 5)) >>> (index & 0x1F)) & 1) == 1
    }
    override def update(index: Int, value: Boolean) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val mask = 1 << (index & 0x1F)
      if (value) words(1 + (index >> 5)) |=  mask
      else       words(1 + (index >> 5)) &= ~mask
    }
    override def copy(length: Int): BitArray = {
      val newWords = new Array[Int](1 + (((length + 31) & ~31) >> 5))
      Array.copy(words, 1, newWords, 1, math.min(words.length, newWords.length) - 1)
      newWords(0) = length
      new BitArray(newWords)
    }
  }
  
  final class ValArrayBuilder[A](implicit typeA: ValType[A]) extends Collector[Any, A] {
    override type Product = ValArray[A]
    private[this] var array: ValArray[A] = ValArray[A](16)
    private[this] var aliased: Boolean = false
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: A) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: ValArray[A] = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = ValArray[A](16)
      aliased = false
      length = 0
    }
  }
  
  final class RefArrayBuilder[A] extends Collector[Any, A] {
    override type Product = RefArray[A]
    private[this] var array: RefArray[A] = RefArray.Empty[A]
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: A) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: RefArray[A] = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = RefArray.Empty[A]
      aliased = true
      length = 0
    }
  }
  
  final class ByteArrayBuilder extends Collector[Any, Byte] {
    override type Product = ByteArray
    private[this] var array: ByteArray = ByteArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Byte) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: ByteArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = ByteArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class ShortArrayBuilder extends Collector[Any, Short] {
    override type Product = ShortArray
    private[this] var array: ShortArray = ShortArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Short) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: ShortArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = ShortArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class IntArrayBuilder extends Collector[Any, Int] {
    override type Product = IntArray
    private[this] var array: IntArray = IntArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Int) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: IntArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = IntArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class LongArrayBuilder extends Collector[Any, Long] {
    override type Product = LongArray
    private[this] var array: LongArray = LongArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Long) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: LongArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = LongArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class FloatArrayBuilder extends Collector[Any, Float] {
    override type Product = FloatArray
    private[this] var array: FloatArray = FloatArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Float) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: FloatArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = FloatArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class DoubleArrayBuilder extends Collector[Any, Double] {
    override type Product = DoubleArray
    private[this] var array: DoubleArray = DoubleArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Double) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: DoubleArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = DoubleArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class CharArrayBuilder extends Collector[Any, Char] {
    override type Product = CharArray
    private[this] var array: CharArray = CharArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Char) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: CharArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = CharArray.Empty
      aliased = true
      length = 0
    }
  }
  
  final class BitArrayBuilder extends Collector[Any, Boolean] {
    override type Product = BitArray
    private[this] var array: BitArray = BitArray.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    private[this] def prepare(size: Int): Unit = if (aliased || size > array.length) {
      array = array.copy(Collector.nextSize(16, size))
      aliased = false
    }
    override def expect(count: Int): Unit = if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    override def += (value: Boolean) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    override def result: BitArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    override def clear() {
      array = BitArray.Empty
      aliased = true
      length = 0
    }
  }
}

private[container] object FlatBuilders {
  import Flat._
  class RefArrayBuilders {
    implicit def RefArrayBuilder[A]: RefArrayBuilder[A] = new RefArrayBuilder[A]
  }
  class ValArrayBuilders extends RefArrayBuilders {
    implicit def ValArrayBuilder[A](implicit typeA: ValType[A]): ValArrayBuilder[A] = new ValArrayBuilder[A]
  }
  class ArrayBuilders extends ValArrayBuilders {
    implicit def ByteArrayBuilder: ByteArrayBuilder = new ByteArrayBuilder
    implicit def ShortArrayBuilder: ShortArrayBuilder = new ShortArrayBuilder
    implicit def IntArrayBuilder: IntArrayBuilder = new IntArrayBuilder
    implicit def LongArrayBuilder: LongArrayBuilder = new LongArrayBuilder
    implicit def FloatArrayBuilder: FloatArrayBuilder = new FloatArrayBuilder
    implicit def DoubleArrayBuilder: DoubleArrayBuilder = new DoubleArrayBuilder
    implicit def CharArrayBuilder: CharArrayBuilder = new CharArrayBuilder
    implicit def BitArrayBuilder: BitArrayBuilder = new BitArrayBuilder
  }
}
