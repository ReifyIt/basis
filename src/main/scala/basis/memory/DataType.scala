/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.annotation.implicitNotFound
import scala.math.max

/** Represents either a [[basis.memory.ValType]] or a [[basis.memory.ReferenceType]].
  * 
  * @tparam T   the modeled instance type.
  */
@implicitNotFound("no implicit data type for ${T}.")
sealed abstract class DataType[T]

/** A data type that conforms to store-by-reference semantics.
  * 
  * @tparam T   the modeled instance type.
  */
@implicitNotFound("no implicit reference type for ${T}.")
final class RefType[T] extends DataType[T] {
  override def toString: String = "ReferenceType"
}

/** A data type that conforms to store-by-value semantics. Value types convert
  * between fixed-length byte sequences and class instances.
  * 
  * ==Frames==
  * A value type's `alignment`, `offset` and `size` constitute its ''frame''.
  * The alignment must evenly divide the offset and size. Value types must
  * never access more than `size - 1` bytes beyond a given address, but should
  * assume proper alignment of provided addresses.
  * 
  * ==Projections==
  * A value type may ''project'' itself to some offset of a larger, alternately
  * aligned frame. ''Projections'' give uniform accees to individual fields, or
  * even sets of fields, of composite value types.
  * 
  * @author Chris Sachs
  * 
  * @tparam T   the modeled instance type.
  * @see  [[basis.memory.Data]]
  */
@implicitNotFound("no implicit value type for ${T}.")
abstract class ValType[@specialized T] extends DataType[T] with Framed[ValType[T]] { self =>
  /** Returns the power-of-two alignment of this type's frame. The alignment
    * must evenly divide all addresses used to store this type's values. */
  def alignment: Int
  
  /** Returns the number of leading padding bytes in this type's frame.
    * The type's alignment must evenly divide its padded offset. */
  def offset: Int
  
  /** Returns the size in bytes of this type's frame. The type's alignmemt
    * must evenly divide its size. */
  def size: Int
  
  /** Loads an instance from a data value.
    * 
    * @param  data      the `Data` to load from.
    * @param  address   the aligned address to load from.
    * @return the loaded instance.
    */
  def load(data: Data, address: Long): T
  
  /** Stores an instance as a data value.
    * 
    * @param  data      the `Data` to store to.
    * @param  address   the aligned address to store to.
    * @param  value     the instance to store.
    */
  def store(data: Data, address: Long, value: T): Unit
  
  /** Returns a view of this type projected into a different frame.
    * 
    * @param  offset      the preferred offset of the new frame.
    * @param  size        the preferred size of the new frame.
    * @param  alignment   the preferred alignment of the new frame.
    * @return the projected view of this type.
    */
  override def framed(offset: Int = this.offset, size: Int = this.size, alignment: Int = this.alignment): Frame =
    new Projection(offset, size, alignment)
  
  /** Returns a formatted string detailing this type's frame as it differs from some default frame. */
  protected def toString(prefix: String, defaultOffset: Int, defaultSize: Int, defaultAlignment: Int): String = {
    val s = new java.lang.StringBuilder(prefix)
    if (offset != defaultOffset || size != defaultSize || alignment != defaultAlignment) {
      var needsSeparator = false
      s.append('.').append("framed").append('(')
      if (offset != defaultOffset) {
        s.append("offset").append(" = ").append(offset)
        needsSeparator = true
      }
      if (size != defaultSize) {
        if (needsSeparator) s.append(", ")
        s.append("size").append(" = ").append(size)
        needsSeparator = true
      }
      if (alignment != defaultAlignment) {
        if (needsSeparator) s.append(", ")
        s.append("alignment").append(" = ").append(alignment)
      }
      s.append(')')
    }
    s.toString
  }
  
  /** A projection of this value type into a different frame.
    * 
    * @param frameOffset     the preferred offset of the projected frame.
    * @param frameSize       the preferred size of the projected frame.
    * @param frameAlignment  the preferred alignment of the projected frame.
    */
  private final class Projection(frameOffset: Int, frameSize: Int, frameAlignment: Int) extends ValType[T] {
    /** Returns this projection's offset into the base type's frame.
      * Aligns `frameOffset` to the base type's alignment. */
    override val offset: Int = align(self.alignment)(frameOffset)
    
    /** Returns the power-of-two alignment of this projection's frame.
      * Aligns `frameAlignment` to the base type's alignment. */
    override val alignment: Int = align(self.alignment)(max(self.alignment, frameAlignment))
    
    /** Returns the size in bytes of this projection's frame. Ensures that
      * `frameSize` completely covers the base type's offset frame, and aligns
      * that size to this type's alignment. */
    override val size: Int = align(alignment)(max(offset + self.size, frameSize))
    
    override def load(data: Data, address: Long): T = self.load(data, address + offset)
    
    override def store(data: Data, address: Long, value: T): Unit = self.store(data, address + offset, value)
    
    override def framed(offset: Int, size: Int, alignment: Int): Frame = new self.Projection(offset, size, alignment)
    
    override def toString: String = toString(self.toString, self.offset, self.size, self.alignment)
  }
}

/** Contains fallback implicit reference types. */
abstract class LowPriorityDataTypes {
  private val RefType = new RefType[Null]
  
  /** Returns a reference type. */
  implicit def Reference[T] = RefType.asInstanceOf[RefType[T]]
}

/** Contains fundamental data types, including implicit value types for
  * primitive Scala types and tuples of value types. */
object DataType extends LowPriorityDataTypes {
  /** Returns the default type of `Byte` values. */
  implicit val PackedByte = new PackedByte(0, 1, 1)
  
  /** Returns the default unaligned type of `Short` values. */
  val PackedShort = new PackedShort(0, 2, 1)
  
  /** Returns the default unaligned type of `Int` values. */
  val PackedInt = new PackedInt(0, 4, 1)
  
  /** Returns the default unaligned type of `Long` values. */
  val PackedLong = new PackedLong(0, 8, 1)
  
  /** Returns the default unaligned type of `Char` values. */
  val PackedChar = new PackedChar(0, 2, 1)
  
  /** Returns the default unaligned type of `Float` values. */
  val PackedFloat = new PackedFloat(0, 4, 1)
  
  /** Returns the default unaligned type of `Double` values. */
  val PackedDouble = new PackedDouble(0, 8, 1)
  
  /** Returns the default type of `Boolean` values. */
  implicit val PackedBoolean = new PackedBoolean(0, 1, 1)
  
  /** Returns the default aligned type of `Short` values. */
  implicit val PaddedShort = new PaddedShort(0, 2, 2)
  
  /** Returns the default aligned type of `Int` values. */
  implicit val PaddedInt = new PaddedInt(0, 4, 4)
  
  /** Returns the default aligned type of `Long` values. */
  implicit val PaddedLong = new PaddedLong(0, 8, 8)
  
  /** Returns the default aligned type of `Char` values. */
  implicit val PaddedChar = new PaddedChar(0, 2, 2)
  
  /** Returns the default aligned type of `Float` values. */
  implicit val PaddedFloat = new PaddedFloat(0, 4, 4)
  
  /** Returns the default aligned type of `Double` values. */
  implicit val PaddedDouble = new PaddedDouble(0, 8, 8)
  
  /** Returns a type for some kind of 2-tuple of values. */
  implicit def Row2[T1 : ValType, T2 : ValType] = new Row2[T1, T2]
  
  /** Returns a type for some kind of 3-tuple of values. */
  implicit def Row3[T1: ValType, T2 : ValType, T3 : ValType] = new Row3[T1, T2, T3]
  
  /** Returns a type for some kind of 4-tuple of values. */
  implicit def Row4[T1: ValType, T2 : ValType, T3 : ValType, T4 : ValType] = new Row4[T1, T2, T3, T4]
  
  /** A type for `Byte` values. */
  final class PackedByte(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Byte] with Framed[PackedByte] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 1, frameSize))
    override def load(data: Data, address: Long): Byte = data.loadByte(address + offset)
    override def store(data: Data, address: Long, value: Byte): Unit = data.storeByte(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedByte = new PackedByte(offset, size, alignment)
    override def toString: String = toString("PackedByte", 0, 1, 1)
  }
  
  /** An unaligned type for `Short` values. */
  final class PackedShort(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Short] with Framed[PackedShort] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 2, frameSize))
    override def load(data: Data, address: Long): Short = data.loadUnalignedShort(address + offset)
    override def store(data: Data, address: Long, value: Short): Unit = data.storeUnalignedShort(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedShort = new PackedShort(offset, size, alignment)
    override def toString: String = toString("PackedShort", 0, 2, 1)
  }
  
  /** An unaligned type for `Int` values. */
  final class PackedInt(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Int] with Framed[PackedInt] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 4, frameSize))
    override def load(data: Data, address: Long): Int = data.loadUnalignedInt(address + offset)
    override def store(data: Data, address: Long, value: Int): Unit = data.storeUnalignedInt(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedInt = new PackedInt(offset, size, alignment)
    override def toString: String = toString("PackedInt", 0, 4, 1)
  }
  
  /** An unaligned type for `Long` values. */
  final class PackedLong(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Long] with Framed[PackedLong] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment)) 
    override val offset: Int = max(0, frameOffset) 
    override val size: Int = align(alignment)(max(offset + 8, frameSize))
    override def load(data: Data, address: Long): Long = data.loadUnalignedLong(address + offset)
    override def store(data: Data, address: Long, value: Long): Unit = data.storeUnalignedLong(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedLong = new PackedLong(offset, size, alignment)
    override def toString: String = toString("PackedLong", 0, 8, 1)
  }
  
  /** An unaligned type for `Char` values. */
  final class PackedChar(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Char] with Framed[PackedChar] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 2, frameSize))
    override def load(data: Data, address: Long): Char = data.loadUnalignedChar(address + offset)
    override def store(data: Data, address: Long, value: Char): Unit = data.storeUnalignedChar(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedChar = new PackedChar(offset, size, alignment)
    override def toString: String = toString("PackedChar", 0, 2, 1)
  }
  
  /** An unaligned type for `Float` values. */
  final class PackedFloat(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Float] with Framed[PackedFloat] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 4, frameSize))
    override def load(data: Data, address: Long): Float = data.loadUnalignedFloat(address + offset)
    override def store(data: Data, address: Long, value: Float): Unit = data.storeUnalignedFloat(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedFloat = new PackedFloat(offset, size, alignment)
    override def toString: String = toString("PackedFloat", 0, 4, 1)
  }
  
  /** An unaligned type for `Double` values. */
  final class PackedDouble(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Double] with Framed[PackedDouble] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 8, frameSize))
    override def load(data: Data, address: Long): Double = data.loadUnalignedDouble(address + offset)
    override def store(data: Data, address: Long, value: Double): Unit = data.storeUnalignedDouble(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PackedDouble = new PackedDouble(offset, size, alignment)
    override def toString: String = toString("PackedDouble", 0, 8, 1)
  }
  
  /** A type for single byte `Boolean` values. Maps non-zero values to `true` and zero to `false`. */
  final class PackedBoolean(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Boolean] with Framed[PackedBoolean] {
    override val alignment: Int = max(1, align(frameAlignment)(frameAlignment))
    override val offset: Int = max(0, frameOffset)
    override val size: Int = align(alignment)(max(offset + 1, frameSize))
    /** Returns `true` if the loaded byte is non-zero. */
    override def load(data: Data, address: Long): Boolean =
      data.loadByte(address + offset) != 0
    /** Stores `true` as `1` and `false` as `0`. */
    override def store(data: Data, address: Long, value: Boolean): Unit =
      data.storeByte(address + offset, if (value) 1.toByte else 0.toByte)
    override def framed(offset: Int, size: Int, alignment: Int): PackedBoolean = new PackedBoolean(offset, size, alignment)
    override def toString: String = toString("PackedBoolean", 0, 1, 1)
  }
  
  /** An aligned type for `Short` values. */
  final class PaddedShort(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Short] with Framed[PaddedShort] {
    override val alignment: Int = align(2)(max(2, frameAlignment))
    override val offset: Int = align(2)(max(0, frameOffset))
    override val size: Int = align(alignment)(max(offset + 2, frameSize))
    override def load(data: Data, address: Long): Short = data.loadShort(address + offset)
    override def store(data: Data, address: Long, value: Short): Unit = data.storeShort(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PaddedShort = new PaddedShort(offset, size, alignment)
    override def toString: String = toString("PaddedShort", 0, 2, 2)
  }
  
  /** An aligned type for `Int` values. */
  final class PaddedInt(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Int] with Framed[PaddedInt] {
    override val alignment: Int = align(4)(max(4, frameAlignment))
    override val offset: Int = align(4)(max(0, frameOffset))
    override val size: Int = align(alignment)(max(offset + 4, frameSize))
    override def load(data: Data, address: Long): Int = data.loadInt(address + offset)
    override def store(data: Data, address: Long, value: Int): Unit = data.storeInt(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PaddedInt = new PaddedInt(offset, size, alignment)
    override def toString: String = toString("PaddedInt", 0, 4, 4)
  }
  
  /** An aligned type for `Long` values. */
  final class PaddedLong(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Long] with Framed[PaddedLong] {
    override val alignment: Int = align(8)(max(8, frameAlignment))
    override val offset: Int = align(8)(max(0, frameOffset))
    override val size: Int = align(alignment)(max(offset + 8, frameSize))
    override def load(data: Data, address: Long): Long = data.loadLong(address + offset)
    override def store(data: Data, address: Long, value: Long): Unit = data.storeLong(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PaddedLong = new PaddedLong(offset, size, alignment)
    override def toString: String = toString("PaddedLong", 0, 8, 8)
  }
  
  /** An aligned type for `Char` values. */
  final class PaddedChar(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Char] with Framed[PaddedChar] {
    override val alignment: Int = align(2)(max(2, frameAlignment))
    override val offset: Int = align(2)(max(0, frameOffset))
    override val size: Int = align(alignment)(max(offset + 2, frameSize))
    override def load(data: Data, address: Long): Char = data.loadChar(address + offset)
    override def store(data: Data, address: Long, value: Char): Unit = data.storeChar(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PaddedChar = new PaddedChar(offset, size, alignment)
    override def toString: String = toString("PaddedChar", 0, 2, 2)
  }
  
  /** An aligned type for `Float` values. */
  final class PaddedFloat(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Float] with Framed[PaddedFloat] {
    override val alignment: Int = align(4)(max(4, frameAlignment))
    override val offset: Int = align(4)(max(0, frameOffset))
    override val size: Int = align(alignment)(max(offset + 4, frameSize))
    override def load(data: Data, address: Long): Float = data.loadFloat(address + offset)
    override def store(data: Data, address: Long, value: Float): Unit = data.storeFloat(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PaddedFloat = new PaddedFloat(offset, size, alignment)
    override def toString: String = toString("PaddedFloat", 0, 4, 4)
  }
  
  /** An aligned type for `Double` values. */
  final class PaddedDouble(frameOffset: Int, frameSize: Int, frameAlignment: Int)
      extends ValType[Double] with Framed[PaddedDouble] {
    override val alignment: Int = align(8)(max(8, frameAlignment))
    override val offset: Int = align(8)(max(0, frameOffset))
    override val size: Int = align(alignment)(max(offset + 8, frameSize))
    override def load(data: Data, address: Long): Double = data.loadDouble(address + offset)
    override def store(data: Data, address: Long, value: Double): Unit = data.storeDouble(address + offset, value)
    override def framed(offset: Int, size: Int, alignment: Int): PaddedDouble = new PaddedDouble(offset, size, alignment)
    override def toString: String = toString("PaddedDouble", 0, 8, 8)
  }
  
  /** A type for some kind of 2-tuple of values. */
  final class Row2[@specialized(Int, Long, Double) T1, @specialized(Int, Long, Double) T2] private (
      protected val column1: ValType[T1],
      protected val column2: ValType[T2],
      frameOffset: Int, frameSize: Int, frameAlignment: Int)
    extends Struct2[ValType[T1], ValType[T2], (T1, T2)](
                    frameOffset, frameSize, frameAlignment)(
                    column1, column2)
      with Framed[Row2[T1, T2]] {
    
    def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
            (implicit column1: ValType[T1], column2: ValType[T2]) =
      this(column1, column2, frameOffset, frameSize, frameAlignment)
    
    def this()(implicit column1: ValType[T1], column2: ValType[T2]) =
      this(column1, column2, 0, 0, 0)
    
    def _1: Field1 = field1
    
    def _2: Field2 = field2
    
    override def load(data: Data, address: Long): (T1, T2) = {
      val arg1 = field1.load(data, address)
      val arg2 = field2.load(data, address)
      (arg1, arg2)
    }
    
    override def store(data: Data, address: Long, tuple: (T1, T2)) {
      field1.store(data, address, tuple._1)
      field2.store(data, address, tuple._2)
    }
    
    override def framed(offset: Int, size: Int, alignment: Int): Row2[T1, T2] =
      new Row2[T1, T2](offset, size, alignment)(column1, column2)
    
    override def toString: String = {
      val prefix = "Row2"+"("+ column1 +", "+ column2 +")"
      toString(prefix, 0, 0, 0)
    }
  }
  
  /** A type for some kind of 3-tuple of values. */
  final class Row3[T1, T2, T3] private (
      protected val column1: ValType[T1],
      protected val column2: ValType[T2],
      protected val column3: ValType[T3],
      frameOffset: Int, frameSize: Int, frameAlignment: Int)
    extends Struct3[ValType[T1], ValType[T2], ValType[T3], (T1, T2, T3)](
                    frameOffset, frameSize, frameAlignment)(
                    column1, column2, column3)
      with Framed[Row3[T1, T2, T3]] {
    
    def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
            (implicit column1: ValType[T1], column2: ValType[T2], column3: ValType[T3]) =
      this(column1, column2, column3, frameOffset, frameSize, frameAlignment)
    
    def this()(implicit column1: ValType[T1], column2: ValType[T2], column3: ValType[T3]) =
      this(column1, column2, column3, 0, 0, 0)
    
    def _1: Field1 = field1
    
    def _2: Field2 = field2
    
    def _3: Field3 = field3
    
    override def load(data: Data, address: Long): (T1, T2, T3) = {
      val arg1 = field1.load(data, address)
      val arg2 = field2.load(data, address)
      val arg3 = field3.load(data, address)
      (arg1, arg2, arg3)
    }
    
    override def store(data: Data, address: Long, tuple: (T1, T2, T3)) {
      field1.store(data, address, tuple._1)
      field2.store(data, address, tuple._2)
      field3.store(data, address, tuple._3)
    }
    
    override def framed(offset: Int, size: Int, alignment: Int): Row3[T1, T2, T3] =
      new Row3[T1, T2, T3](offset, size, alignment)(column1, column2, column3)
    
    override def toString: String = {
      val prefix = "Row3"+"("+ column1 +", "+ column2 +", "+ column3 +")"
      toString(prefix, 0, 0, 0)
    }
  }
  
  /** A type for some kind of 4-tuple of values. */
  final class Row4[T1, T2, T3, T4] private (
      protected val column1: ValType[T1],
      protected val column2: ValType[T2],
      protected val column3: ValType[T3],
      protected val column4: ValType[T4],
      frameOffset: Int, frameSize: Int, frameAlignment: Int)
    extends Struct4[ValType[T1], ValType[T2], ValType[T3], ValType[T4], (T1, T2, T3, T4)](
                    frameOffset, frameSize, frameAlignment)(
                    column1, column2, column3, column4)
      with Framed[Row4[T1, T2, T3, T4]] {
    
    def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
            (implicit column1: ValType[T1], column2: ValType[T2], column3: ValType[T3], column4: ValType[T4]) =
      this(column1, column2, column3, column4, frameOffset, frameSize, frameAlignment)
    
    def this()(implicit column1: ValType[T1], column2: ValType[T2], column3: ValType[T3], column4: ValType[T4]) =
      this(column1, column2, column3, column4, 0, 0, 0)
    
    def _1: Field1 = field1
    
    def _2: Field2 = field2
    
    def _3: Field3 = field3
    
    def _4: Field4 = field4
    
    override def load(data: Data, address: Long): (T1, T2, T3, T4) = {
      val arg1 = field1.load(data, address)
      val arg2 = field2.load(data, address)
      val arg3 = field3.load(data, address)
      val arg4 = field4.load(data, address)
      (arg1, arg2, arg3, arg4)
    }
    
    override def store(data: Data, address: Long, tuple: (T1, T2, T3, T4)) {
      field1.store(data, address, tuple._1)
      field2.store(data, address, tuple._2)
      field3.store(data, address, tuple._3)
      field4.store(data, address, tuple._4)
    }
    
    override def framed(offset: Int, size: Int, alignment: Int): Row4[T1, T2, T3, T4] =
      new Row4[T1, T2, T3, T4](offset, size, alignment)(column1, column2, column3, column4)
    
    override def toString: String = {
      val prefix = "Row4"+"("+ column1 +", "+ column2 +", "+ column3 +", "+ column4 +")"
      toString(prefix, 0, 0, 0)
    }
  }
}
