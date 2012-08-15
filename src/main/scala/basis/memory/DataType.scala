/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Represents either a [[basis.memory.ValType]] or a [[basis.memory.RefType]].
  * 
  * @tparam T   the modeled instance type.
  */
@scala.annotation.implicitNotFound("no implicit data type for ${T}")
sealed abstract class DataType[T]

/** Contains fundamental data types, including implicit value types for
  * primitive Scala types and tuples of value types. */
object DataType extends ValTypes {
  def apply[T](implicit datatype: DataType[T]): datatype.type = datatype
}

/** A data type that conforms to store-by-reference semantics.
  * 
  * @tparam T   the modeled instance type.
  */
@scala.annotation.implicitNotFound("no implicit reference type for ${T}")
final class RefType[T] extends DataType[T] {
  override def toString: String = "ReferenceType"
}

object RefType {
  private[this] val Reference = new RefType[Nothing]
  /** Returns a reference type. */
  implicit def apply[T]: RefType[T] = Reference.asInstanceOf[RefType[T]]
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
@scala.annotation.implicitNotFound("no implicit value type for ${T}")
abstract class ValType[T] extends DataType[T] {
  /** Returns the power-of-two alignment of this type's frame. The alignment
    * must evenly divide all addresses used to store this type's values. */
  def alignment: Long
  
  /** Returns the size in bytes of this type's frame. The type's alignmemt
    * must evenly divide its size. */
  def size: Long
  
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
}

object ValType {
  import java.lang.Math.max
  
  def apply[T](implicit valtype: ValType[T]): valtype.type = valtype
  
  object PackedByte extends ValType[Byte] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 1L
    @inline override def load(data: Data, address: Long): Byte =
      data.loadByte(address)
    @inline override def store(data: Data, address: Long, value: Byte): Unit =
      data.storeByte(address, value)
    override def toString: String = "PackedByte"
  }
  
  object PackedShort extends ValType[Short] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 2L
    @inline override def load(data: Data, address: Long): Short =
      data.loadUnalignedShort(address)
    @inline override def store(data: Data, address: Long, value: Short): Unit =
      data.storeUnalignedShort(address, value)
    override def toString: String = "PackedShort"
  }
  
  object PackedInt extends ValType[Int] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 4L
    @inline override def load(data: Data, address: Long): Int =
      data.loadUnalignedInt(address)
    @inline override def store(data: Data, address: Long, value: Int): Unit =
      data.storeUnalignedInt(address, value)
    override def toString: String = "PackedInt"
  }
  
  object PackedLong extends ValType[Long] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 8L
    @inline override def load(data: Data, address: Long): Long =
      data.loadUnalignedLong(address)
    @inline override def store(data: Data, address: Long, value: Long): Unit =
      data.storeUnalignedLong(address, value)
    override def toString: String = "PackedLong"
  }
  
  object PackedChar extends ValType[Char] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 2L
    @inline override def load(data: Data, address: Long): Char =
      data.loadUnalignedChar(address)
    @inline override def store(data: Data, address: Long, value: Char): Unit =
      data.storeUnalignedChar(address, value)
    override def toString: String = "PackedChar"
  }
  
  object PackedFloat extends ValType[Float] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 4L
    @inline override def load(data: Data, address: Long): Float =
      data.loadUnalignedFloat(address)
    @inline override def store(data: Data, address: Long, value: Float): Unit =
      data.storeUnalignedFloat(address, value)
    override def toString: String = "PackedFloat"
  }
  
  object PackedDouble extends ValType[Double] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 8L
    @inline override def load(data: Data, address: Long): Double =
      data.loadUnalignedDouble(address)
    @inline override def store(data: Data, address: Long, value: Double): Unit =
      data.storeUnalignedDouble(address, value)
    override def toString: String = "PackedDouble"
  }
  
  object PackedBoolean extends ValType[Boolean] {
    @inline override def alignment: Long = 1L
    @inline override def size: Long = 1L
    @inline override def load(data: Data, address: Long): Boolean =
      data.loadByte(address) == 0
    @inline override def store(data: Data, address: Long, value: Boolean): Unit =
      data.storeByte(address, if (value) 0.toByte else -1.toByte)
    override def toString: String = "PackedBoolean"
  }
  
  object PaddedShort extends ValType[Short] {
    @inline override def alignment: Long = 2L
    @inline override def size: Long = 2L
    @inline override def load(data: Data, address: Long): Short =
      data.loadShort(address)
    @inline override def store(data: Data, address: Long, value: Short): Unit =
      data.storeShort(address, value)
    override def toString: String = "PaddedShort"
  }
  
  object PaddedInt extends ValType[Int] {
    @inline override def alignment: Long = 4L
    @inline override def size: Long = 4L
    @inline override def load(data: Data, address: Long): Int =
      data.loadInt(address)
    @inline override def store(data: Data, address: Long, value: Int): Unit =
      data.storeInt(address, value)
    override def toString: String = "PaddedInt"
  }
  
  object PaddedLong extends ValType[Long] {
    @inline override def alignment: Long = 8L
    @inline override def size: Long = 8L
    @inline override def load(data: Data, address: Long): Long =
      data.loadLong(address)
    @inline override def store(data: Data, address: Long, value: Long): Unit =
      data.storeLong(address, value)
    override def toString: String = "PaddedLong"
  }
  
  object PaddedChar extends ValType[Char] {
    @inline override def alignment: Long = 2L
    @inline override def size: Long = 2L
    @inline override def load(data: Data, address: Long): Char =
      data.loadChar(address)
    @inline override def store(data: Data, address: Long, value: Char): Unit =
      data.storeLong(address, value)
    override def toString: String = "PaddedChar"
  }
  
  object PaddedFloat extends ValType[Float] {
    @inline override def alignment: Long = 4L
    @inline override def size: Long = 4L
    @inline override def load(data: Data, address: Long): Float =
      data.loadFloat(address)
    @inline override def store(data: Data, address: Long, value: Float): Unit =
      data.storeFloat(address, value)
    override def toString: String = "PaddedFloat"
  }
  
  object PaddedDouble extends ValType[Double] {
    @inline override def alignment: Long = 8L
    @inline override def size: Long = 8L
    @inline override def load(data: Data, address: Long): Double =
      data.loadDouble(address)
    @inline override def store(data: Data, address: Long, value: Double): Unit =
      data.storeDouble(address, value)
    override def toString: String = "PaddedDouble"
  }
  
  final class Record2[T1, T2](
      field1: ValType[T1], field2: ValType[T2])
    extends ValType[(T1, T2)] {
    private[this] val offset2: Long =
      align(field1.size, field2.alignment)
    override val alignment: Long =
      max(field1.alignment, field2.alignment)
    override val size: Long =
      align(offset2 + field2.size, alignment)
    override def load(data: Data, address: Long): (T1, T2) =
      (field1.load(data, address),
       field2.load(data, address + offset2))
    override def store(data: Data, address: Long, tuple: (T1, T2)) {
      field1.store(data, address,           tuple._1)
      field2.store(data, address + offset2, tuple._2)
    }
    override def toString: String =
      "Record2"+"("+ field1 +", "+ field2 +")"
  }
  
  final class Record3[T1, T2, T3](
      field1: ValType[T1], field2: ValType[T2],
      field3: ValType[T3])
    extends ValType[(T1, T2, T3)] {
    private[this] val offset2: Long =
      align(field1.size, field2.alignment)
    private[this] val offset3: Long =
      align(offset2 + field2.size, field3.alignment)
    override val alignment: Long =
      max(max(field1.alignment, field2.alignment), field3.alignment)
    override val size: Long =
      align(offset3 + field3.size, alignment)
    override def load(data: Data, address: Long): (T1, T2, T3) =
      (field1.load(data, address),
       field2.load(data, address + offset2),
       field3.load(data, address + offset3))
    override def store(data: Data, address: Long, tuple: (T1, T2, T3)) {
      field1.store(data, address,           tuple._1)
      field2.store(data, address + offset2, tuple._2)
      field3.store(data, address + offset3, tuple._3)
    }
    override def toString: String =
      "Record3"+"("+ field1 +", "+ field2 +", "+ field3 +")"
  }
  
  final class Record4[T1, T2, T3, T4](
      field1: ValType[T1], field2: ValType[T2],
      field3: ValType[T3], field4: ValType[T4])
    extends ValType[(T1, T2, T3, T4)] {
    private[this] val offset2: Long =
      align(field1.size, field2.alignment)
    private[this] val offset3: Long =
      align(offset2 + field2.size, field3.alignment)
    private[this] val offset4: Long =
      align(offset3 + field3.size, field4.alignment)
    override val alignment: Long =
      max(max(max(
        field1.alignment, field2.alignment),
        field3.alignment), field4.alignment)
    override val size: Long =
      align(offset4 + field4.size, alignment)
    override def load(data: Data, address: Long): (T1, T2, T3, T4) =
      (field1.load(data, address),
       field2.load(data, address + offset2),
       field3.load(data, address + offset3),
       field4.load(data, address + offset4))
    override def store(data: Data, address: Long, tuple: (T1, T2, T3, T4)) {
      field1.store(data, address,           tuple._1)
      field2.store(data, address + offset2, tuple._2)
      field3.store(data, address + offset3, tuple._3)
      field4.store(data, address + offset4, tuple._4)
    }
    override def toString: String =
      "Record4"+"("+ field1 +", "+ field2 +", "+ field3 +", "+ field4 +")"
  }
}

private[memory] class RefTypes {
  implicit def Reference[T]: RefType[T] = RefType[T]
}

private[memory] class ValTypes extends RefTypes {
  implicit def PackedByte: ValType.PackedByte.type = ValType.PackedByte
  def PackedShort: ValType.PackedShort.type = ValType.PackedShort
  def PackedInt: ValType.PackedInt.type = ValType.PackedInt
  def PackedLong: ValType.PackedLong.type = ValType.PackedLong
  def PackedChar: ValType.PackedChar.type = ValType.PackedChar
  def PackedFloat: ValType.PackedFloat.type = ValType.PackedFloat
  def PackedDouble: ValType.PackedDouble.type = ValType.PackedDouble
  implicit def PackedBoolean: ValType.PackedBoolean.type = ValType.PackedBoolean
  implicit def PaddedShort: ValType.PaddedShort.type = ValType.PaddedShort
  implicit def PaddedInt: ValType.PaddedInt.type = ValType.PaddedInt
  implicit def PaddedLong: ValType.PaddedLong.type = ValType.PaddedLong
  implicit def PaddedChar: ValType.PaddedChar.type = ValType.PaddedChar
  implicit def PaddedFloat: ValType.PaddedFloat.type = ValType.PaddedFloat
  implicit def PaddedDouble: ValType.PaddedDouble.type = ValType.PaddedDouble
  implicit def Record2[T1, T2](
      implicit field1: ValType[T1], field2: ValType[T2])
    : ValType.Record2[T1, T2] =
    new ValType.Record2(field1, field2)
  implicit def Record3[T1, T2, T3](
      implicit field1: ValType[T1], field2: ValType[T2],
               field3: ValType[T3])
    : ValType.Record3[T1, T2, T3] =
    new ValType.Record3(field1, field2, field3)
  implicit def Record4[T1, T2, T3, T4](
      implicit field1: ValType[T1], field2: ValType[T2],
               field3: ValType[T3], field4: ValType[T4])
    : ValType.Record4[T1, T2, T3, T4] =
    new ValType.Record4(field1, field2, field3, field4)
}
