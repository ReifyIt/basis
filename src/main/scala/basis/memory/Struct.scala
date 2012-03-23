/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.annotation.implicitNotFound
import scala.math.max

/** A typeclass that implements store-by-value semantics. Structs convert
  * between fixed-length byte sequences and Scala values. Think of a Struct as
  * the value type of a Scala type.
  * 
  * ==Frames==
  * A Struct's `alignment` and `size` constitute its ''frame''. A Struct's
  * size must be a multiple of its alignment. Structs must never access more
  * than `size - 1` bytes beyond any given address, and should assume that all
  * given addresses are properly aligned.
  * 
  * ==Projections==
  * A Struct may ''project'' itself to an offset of a larger, alternately
  * aligned frame. ''Projections'' give uniform accees to individual fields,
  * or even groups of fields, of aggregate value types.
  * 
  * @author Chris Sachs
  * 
  * @tparam T   The instance type of this value type.
  * @see [[basis.memory.Data]]
  */
@implicitNotFound("No implicit Struct for ${T}")
abstract class Struct[@specialized T] { struct =>
  /** The power-of-two alignment of this Struct's frame. Storage addresses must
    * be multiples of this alignment. */
  def alignment: Long
  
  /** The size in bytes of this Struct's frame. Must be a multiple of `alignment`. */
  def size: Long
  
  /** Loads an instance of a value.
    * 
    * @param  data      the Data to load from.
    * @param  address   the aligned `data` address to load from.
    * @return the loaded instance.
    */
  def load(data: Data, address: Long): T
  
  /** Stores an instance as a value.
    * 
    * @param  data      the Data to store to.
    * @param  address   the aligned `data` address to store to.
    * @param  value     the instance to store.
    */
  def store(data: Data, address: Long, value: T): Unit
  
  /** Projects this Struct into a different frame.
    * 
    * @param  offset      the incremental offset into the new frame.
    * @param  size        the preferred size of the new frame.
    * @param  alignment   the preferred alignment of the new frame.
    * @return A Struct that embeds this value type into the new frame.
    */
  def project(offset: Long = 0L, size: Long = this.size, alignment: Long = this.alignment): Struct[T] =
    new Frame(offset, size, alignment)
  
  /** A projection of the outer Struct into a new frame.
   * 
   * @param frameOffset     the cumulative offset into the frame.
   * @param frameSize       the size of the frame.
   * @param frameAlignment  the alignment of the frame.
   */
  protected class Frame(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[T] {
    /** The cumulative offset into this Struct's frame. Forces `frameOffset` to
      * align to the outer Struct's alignment. */
    val offset: Long = align(struct.alignment)(frameOffset)
    
    /** The alignment of this Struct's frame. Forces `frameAlignment` to a
      * multiple of the outer Struct's alignment. */
    val alignment: Long = align(struct.alignment)(max(struct.alignment, frameAlignment))
    
    /** The size of this Struct's frame. Forces `frameSize` to contain the outer
      * Struct's offset frame and to align to this Struct's alignment. */
    val size: Long = align(alignment)(max(offset + struct.size, frameSize))
    
    def load(data: Data, address: Long): T =
      struct.load(data, address + offset)
    
    def store(data: Data, address: Long, value: T): Unit =
      struct.store(data, address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): Struct[T] =
      new struct.Frame(this.offset + offset, size, alignment)
    
    override def toString: String =
      "%s.project(offset = %d, size = %d, alignment = %d)".format(struct, offset, size, alignment)
  }
}

/** Contains value types for fundamental Scala types. Implicitly provides an
  * ''aligned'' value type for each primitive Scala type. Also implicitly
  * provides value types for tuples of value typed elements.
  */
object Struct {
  /** Returns the implicit value type of an instance type.
    * 
    * Use as shorthand to obtain an implicitly scoped value type.
    * {{{
    * scala> val row = Struct[(Int, Double)]
    * row: basis.memory.Struct[(Int, Double)] = Record2(PaddedInt, PaddedDouble)
    * }}}
    * 
    * @tparam T       the instance type of the value type to retrieve.
    * @param  struct  the implicit value type itself.
    * @return the implicitly supplied value type.
    */
  @inline def apply[T](implicit struct: Struct[T]): Struct[T] = struct
  
  /** Returns the implicit raw type of an instance type. 
    * 
    * @tparam T       the instance type of the raw type to retrieve.
    * @param  struct  the implicit value type if available.
    * @return An option containing some `struct` or none.
    */
  implicit def raw[T](implicit struct: Struct[T] = null): Option[Struct[T]] =
    if (struct != null) Some(struct) else None
  
  /** The value type of `Byte` values. */
  final class PackedByte(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Byte] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 1L, frameSize))
    
    def load(data: Data, address: Long): Byte =
      data.loadByte(address + offset)
    
    def store(data: Data, address: Long, value: Byte): Unit =
      data.storeByte(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedByte =
      new PackedByte(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 1L && alignment == 1L) "PackedByte"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedByte", offset, size, alignment)
    }
  }
  
  /** The default value type of `Byte` values. */
  implicit val PackedByte = new PackedByte
  
  /** The value type of aligned `Short` values. */
  final class PaddedShort(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Short] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = align(2L)(max(0L, frameOffset))
    
    val alignment: Long = align(2L)(max(2L, frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 2L, frameSize))
    
    def load(data: Data, address: Long): Short =
      data.loadShort(address + offset)
    
    def store(data: Data, address: Long, value: Short): Unit =
      data.storeShort(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PaddedShort =
      new PaddedShort(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 2L && alignment == 2L) "PaddedShort"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PaddedShort", offset, size, alignment)
    }
  }
  
  /** The default value type of aligned `Short` values. */
  implicit val PaddedShort = new PaddedShort
  
  /** The value type of aligned `Int` values. */
  final class PaddedInt(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Int] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = align(4L)(max(0L, frameOffset))
    
    val alignment: Long = align(4L)(max(4L, frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 4L, frameSize))
    
    def load(data: Data, address: Long): Int =
      data.loadInt(address + offset)
    
    def store(data: Data, address: Long, value: Int): Unit =
      data.storeInt(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PaddedInt =
      new PaddedInt(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 4L && alignment == 4L) "PaddedInt"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PaddedInt", offset, size, alignment)
    }
  }
  
  /** The default value type of aligned `Int` values. */
  implicit val PaddedInt = new PaddedInt
  
  /** The value type of aligned `Long` values. */
  final class PaddedLong(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Long] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = align(8L)(max(0L, frameOffset))
    
    val alignment: Long = align(8L)(max(8L, frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 8L, frameSize))
    
    def load(data: Data, address: Long): Long =
      data.loadLong(address + offset)
    
    def store(data: Data, address: Long, value: Long): Unit =
      data.storeLong(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PaddedLong =
      new PaddedLong(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 8L && alignment == 8L) "PaddedLong"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PaddedLong", offset, size, alignment)
    }
  }
  
  /** The default value type of aligned `Long` values. */
  implicit val PaddedLong = new PaddedLong
  
  /** The value type of aligned `Char` values. */
  final class PaddedChar(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Char] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = align(2L)(max(0L, frameOffset))
    
    val alignment: Long = align(2L)(max(2L, frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 2L, frameSize))
    
    def load(data: Data, address: Long): Char =
      data.loadChar(address + offset)
    
    def store(data: Data, address: Long, value: Char): Unit =
      data.storeChar(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PaddedChar =
      new PaddedChar(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 2L && alignment == 2L) "PaddedChar"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PaddedChar", offset, size, alignment)
    }
  }
  
  /** The default value type of aligned `Char` values. */
  implicit val PaddedChar = new PaddedChar
  
  /** The value type of aligned `Float` values. */
  final class PaddedFloat(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Float] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = align(4L)(max(0L, frameOffset))
    
    val alignment: Long = align(4L)(max(4L, frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 4L, frameSize))
    
    def load(data: Data, address: Long): Float =
      data.loadFloat(address + offset)
    
    def store(data: Data, address: Long, value: Float): Unit =
      data.storeFloat(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PaddedFloat =
      new PaddedFloat(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 4L && alignment == 4L) "PaddedFloat"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PaddedFloat", offset, size, alignment)
    }
  }
  
  /** The default value type of aligned `Float` values. */
  implicit val PaddedFloat = new PaddedFloat
  
  /** The value type of aligned `Double` values. */
  final class PaddedDouble(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Double] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = align(8L)(max(0L, frameOffset))
    
    val alignment: Long = align(8L)(max(8L, frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 8L, frameSize))
    
    def load(data: Data, address: Long): Double =
      data.loadDouble(address + offset)
    
    def store(data: Data, address: Long, value: Double): Unit =
      data.storeDouble(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PaddedDouble =
      new PaddedDouble(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 8L && alignment == 8L) "PaddedDouble"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PaddedDouble", offset, size, alignment)
    }
  }
  
  /** The default value type of aligned `Double` values. */
  implicit val PaddedDouble = new PaddedDouble
  
  /** The value type of unaligned `Short` values. */
  final class PackedShort(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Short] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 2L, frameSize))
    
    def load(data: Data, address: Long): Short =
      data.loadUnalignedShort(address + offset)
    
    def store(data: Data, address: Long, value: Short): Unit =
      data.storeUnalignedShort(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedShort =
      new PackedShort(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 2L && alignment == 1L) "PackedShort"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedShort", offset, size, alignment)
    }
  }
  
  /** The default value type of unaligned `Short` values. */
  val PackedShort = new PackedShort
  
  /** The value type of unaligned `Int` values. */
  final class PackedInt(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Int] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 4L, frameSize))
    
    def load(data: Data, address: Long): Int =
      data.loadUnalignedInt(address + offset)
    
    def store(data: Data, address: Long, value: Int): Unit =
      data.storeUnalignedInt(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedInt =
      new PackedInt(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 4L && alignment == 1L) "PackedInt"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedInt", offset, size, alignment)
    }
  }
  
  /** The default value type of unaligned `Int` values. */
  val PackedInt = new PackedInt
  
  /** The value type of unaligned `Long` values. */
  final class PackedLong(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Long] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 8L, frameSize))
    
    def load(data: Data, address: Long): Long =
      data.loadUnalignedLong(address + offset)
    
    def store(data: Data, address: Long, value: Long): Unit =
      data.storeUnalignedLong(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedLong =
      new PackedLong(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 8L && alignment == 1L) "PackedLong"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedLong", offset, size, alignment)
    }
  }
  
  /** The default value type of unaligned `Long` values. */
  val PackedLong = new PackedLong
  
  /** The value type of unaligned `Char` values. */
  final class PackedChar(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Char] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 2L, frameSize))
    
    def load(data: Data, address: Long): Char =
      data.loadUnalignedChar(address + offset)
    
    def store(data: Data, address: Long, value: Char): Unit =
      data.storeUnalignedChar(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedChar =
      new PackedChar(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 2L && alignment == 1L) "PackedChar"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedChar", offset, size, alignment)
    }
  }
  
  /** The default value type of unaligned `Char` values. */
  val PackedChar = new PackedChar
  
  /** The value type of unaligned `Float` values. */
  final class PackedFloat(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Float] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 4L, frameSize))
    
    def load(data: Data, address: Long): Float =
      data.loadUnalignedFloat(address + offset)
    
    def store(data: Data, address: Long, value: Float): Unit =
      data.storeUnalignedFloat(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedFloat =
      new PackedFloat(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 4L && alignment == 1L) "PackedFloat"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedFloat", offset, size, alignment)
    }
  }
  
  /** The default value type of unaligned `Float` values. */
  val PackedFloat = new PackedFloat
  
  /** The value type of unaligned `Double` values. */
  final class PackedDouble(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Double] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 8L, frameSize))
    
    def load(data: Data, address: Long): Double =
      data.loadUnalignedDouble(address + offset)
    
    def store(data: Data, address: Long, value: Double): Unit =
      data.storeUnalignedDouble(address + offset, value)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedDouble =
      new PackedDouble(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 8L && alignment == 1L) "PackedDouble"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedDouble", offset, size, alignment)
    }
  }
  
  /** The default value type of unaligned `Double` values. */
  val PackedDouble = new PackedDouble
  
  /** The value type of single byte `Boolean` values. */
  final class PackedBoolean(frameOffset: Long, frameSize: Long, frameAlignment: Long) extends Struct[Boolean] {
    def this() = this(0L, 0L, 0L)
    
    val offset: Long = max(0L, frameOffset)
    
    val alignment: Long = max(1L, align(frameAlignment)(frameAlignment))
    
    val size: Long = align(alignment)(max(offset + 1L, frameSize))
    
    /** Returns `true` if loaded byte is non-zero. */
    def load(data: Data, address: Long): Boolean =
      data.loadByte(address + offset) != 0
    
    /** Stores `true` as `1` and `false` as `0`. */
    def store(data: Data, address: Long, value: Boolean): Unit =
      data.storeByte(address + offset, if (value) 1.toByte else 0.toByte)
    
    override def project(offset: Long, size: Long, alignment: Long): PackedBoolean =
      new PackedBoolean(this.offset + offset, size, alignment)
    
    override def toString: String = {
      if (offset == 0L && size == 1L && alignment == 1L) "PackedBoolean"
      else "%s.project(offset = %d, size = %d, alignment = %d)".format("PackedBoolean", offset, size, alignment)
    }
  }
  
  /** The default value type of `Boolean` values. */
  implicit val PackedBoolean = new PackedBoolean
  
  /** The value type of a kind of 2-tuple. */
  final class Record2[@specialized(Int, Long, Double) T1, @specialized(Int, Long, Double) T2]
      (frameOffset: Long, frameSize: Long, frameAlignment: Long)
      (implicit struct1: Struct[T1], struct2: Struct[T2])
    extends Struct2[T1, T2, (T1, T2)](frameOffset, frameSize, frameAlignment) {
    
    def this()(implicit struct1: Struct[T1], struct2: Struct[T2]) = this(0L, 0L, 0L)
    
    def apply(_1: T1, _2: T2) = (_1, _2)
    
    def unapply(value: (T1, T2)) = Tuple2.unapply(value)
    
    override def load(data: Data, address: Long): (T1, T2) = {
      val _1 = field1.load(data, address)
      val _2 = field2.load(data, address)
      apply(_1, _2)
    }
    
    override def store(data: Data, address: Long, value: (T1, T2)) {
      field1.store(data, address, value._1)
      field2.store(data, address, value._2)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): Record2[T1, T2] =
      new Record2[T1, T2](offset1 + offset, size, alignment)(column1, column2)
    
    override def toString: String =
      "Record2"+"("+ column1 +", "+ column2 +")"
  }
  
  /** Returns the value type of a kind of 2-tuple.
    * 
    * @tparam T1        the instance type of the first tuple component.
    * @tparam T2        the instance type of the second tuple component.
    * @return the value type of the kind of tuple.
    */
  implicit def Record2[@specialized(Int, Long, Double) T1 : Struct, @specialized(Int, Long, Double) T2 : Struct] =
    new Record2[T1, T2]
  
  /** The value type of a kind of 3-tuple. */
  final class Record3[T1, T2, T3]
      (frameOffset: Long, frameSize: Long, frameAlignment: Long)
      (implicit struct1: Struct[T1], struct2: Struct[T2], struct3: Struct[T3])
    extends Struct3[T1, T2, T3, (T1, T2, T3)](frameOffset, frameSize, frameAlignment) {
    
    def this()(implicit struct1: Struct[T1], struct2: Struct[T2], struct3: Struct[T3]) = this(0L, 0L, 0L)
    
    def apply(_1: T1, _2: T2, _3: T3) = (_1, _2, _3)
    
    def unapply(value: (T1, T2, T3)) = Tuple3.unapply(value)
    
    override def store(data: Data, address: Long, value: (T1, T2, T3)) {
      field1.store(data, address, value._1)
      field2.store(data, address, value._2)
      field3.store(data, address, value._3)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): Record3[T1, T2, T3] =
      new Record3(offset1 + offset, size, alignment)(column1, column2, column3)
    
    override def toString: String =
      "Record3"+"("+ column1 +", "+ column2 +", "+ column3 +")"
  }
  
  /** Returns the value type of a kind of 3-tuple.
    * 
    * @tparam T1        the instance type of the first tuple component.
    * @tparam T2        the instance type of the second tuple component.
    * @tparam T3        the instance type of the third tuple component.
    * @return the value type of the kind of tuple.
    */
  implicit def Record3[T1 : Struct, T2 : Struct, T3 : Struct] =
    new Record3[T1, T2, T3]
  
  /** The value type of a kind of 4-tuple. */
  final class Record4[T1, T2, T3, T4]
      (frameOffset: Long, frameSize: Long, frameAlignment: Long)
      (implicit struct1: Struct[T1], struct2: Struct[T2], struct3: Struct[T3], struct4: Struct[T4])
    extends Struct4[T1, T2, T3, T4, (T1, T2, T3, T4)](frameOffset, frameSize, frameAlignment) {
    
    def this()(implicit struct1: Struct[T1], struct2: Struct[T2], struct3: Struct[T3], struct4: Struct[T4]) = this(0L, 0L, 0L)
    
    def apply(_1: T1, _2: T2, _3: T3, _4: T4) = (_1, _2, _3, _4)
    
    def unapply(value: (T1, T2, T3, T4)) = Tuple4.unapply(value)
    
    override def store(data: Data, address: Long, value: (T1, T2, T3, T4)) {
      field1.store(data, address, value._1)
      field2.store(data, address, value._2)
      field3.store(data, address, value._3)
      field4.store(data, address, value._4)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): Record4[T1, T2, T3, T4] =
      new Record4(offset1 + offset, size, alignment)(column1, column2, column3, column4)
    
    override def toString: String =
      "Record4"+"("+ column1 +", "+ column2 +", "+ column3 +", "+ column4 +")"
  }
  
  /** Returns the value type of a kind of 4-tuple.
    * 
    * @tparam T1        the instance type of the first tuple component.
    * @tparam T2        the instance type of the second tuple component.
    * @tparam T3        the instance type of the third tuple component.
    * @tparam T4        the instance type of the fourth tuple component.
    * @return the value type of the kind of tuple.
    */
  implicit def Record4[T1 : Struct, T2 : Struct, T3 : Struct, T4 : Struct] =
    new Record4[T1, T2, T3, T4]
}
