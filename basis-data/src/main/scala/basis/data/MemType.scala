/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala.annotation.implicitNotFound

/** A memory storage strategy. Each memory type is either
  * a [[basis.data.RefType]] or a [[basis.data.ValType]].
  * 
  * @tparam T   the modeled instance type.
  */
@implicitNotFound("no implicit memory type for ${T}")
sealed abstract class MemType[T]

/** A factory for fundamental memory types. Includes implicit value types
  * for primitive Scala types and tuples of value types. */
object MemType extends ValTypes {
  def apply[T](implicit memtype: MemType[T]): memtype.type = memtype
}

/** A store-by-reference memory strategy.
  * 
  * @tparam T   the modeled instance type.
  */
@implicitNotFound("no implicit reference type for ${T}")
final class RefType[T] extends MemType[T] {
  override def toString: String = "ReferenceType"
}

/** A factory for reference types.  */
object RefType {
  private[this] val Reference = new RefType[Nothing]
  
  implicit def apply[T]: RefType[T] = Reference.asInstanceOf[RefType[T]]
}

/** A store-by-value memory strategy. Value types define an isomorphism
  * between fixed-length byte sequences and class instances.
  * 
  * ==Frames==
  * A value type's `alignment`, `offset` and `size` constitute its ''frame'';
  * the alignment must evenly divide the offset and size. Value types must
  * never access more than `size - 1` bytes beyond a given address. Assume
  * proper alignment of provided addresses.
  * 
  * @author Chris Sachs
  * 
  * @tparam T   the modeled instance type.
  */
@implicitNotFound("no implicit value type for ${T}")
abstract class ValType[T] extends MemType[T] {
  /** Returns the power-of-two alignment of this type's frame. The alignment
    * must evenly divide all addresses used to store this type's values. */
  def alignment: Long
  
  /** Returns the size in bytes of this type's frame. The type's alignmemt
    * must evenly divide this size. */
  def size: Long
  
  /** Loads an instance from a memory value.
    * 
    * @param  mem       the memory to load from.
    * @param  address   the aligned address in `mem` to load from.
    * @return the loaded instance.
    */
  def load(mem: Mem, address: Long): T
  
  /** Stores an instance as a memory value.
    * 
    * @param  mem       the memory to store to.
    * @param  address   the aligned address in `mem` to store to.
    * @param  value     the instance to store.
    */
  def store(mem: Mem, address: Long, value: T): Unit
}

/** A factory for fundamental value types. */
object ValType {
  import java.lang.Math.max
  
  def apply[T](implicit valtype: ValType[T]): valtype.type = valtype
  
  object PackedByte extends ValType[Byte] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(mem: Mem, address: Long): Byte =
      mem.loadByte(address)
    override def store(mem: Mem, address: Long, value: Byte): Unit =
      mem.storeByte(address, value)
    override def toString: String = "PackedByte"
  }
  
  object PackedShort extends ValType[Short] {
    override def alignment: Long = 1L
    override def size: Long = 2L
    override def load(mem: Mem, address: Long): Short =
      mem.loadUnalignedShort(address)
    override def store(mem: Mem, address: Long, value: Short): Unit =
      mem.storeUnalignedShort(address, value)
    override def toString: String = "PackedShort"
  }
  
  object PackedInt extends ValType[Int] {
    override def alignment: Long = 1L
    override def size: Long = 4L
    override def load(mem: Mem, address: Long): Int =
      mem.loadUnalignedInt(address)
    override def store(mem: Mem, address: Long, value: Int): Unit =
      mem.storeUnalignedInt(address, value)
    override def toString: String = "PackedInt"
  }
  
  object PackedLong extends ValType[Long] {
    override def alignment: Long = 1L
    override def size: Long = 8L
    override def load(mem: Mem, address: Long): Long =
      mem.loadUnalignedLong(address)
    override def store(mem: Mem, address: Long, value: Long): Unit =
      mem.storeUnalignedLong(address, value)
    override def toString: String = "PackedLong"
  }
  
  object PackedFloat extends ValType[Float] {
    override def alignment: Long = 1L
    override def size: Long = 4L
    override def load(mem: Mem, address: Long): Float =
      mem.loadUnalignedFloat(address)
    override def store(mem: Mem, address: Long, value: Float): Unit =
      mem.storeUnalignedFloat(address, value)
    override def toString: String = "PackedFloat"
  }
  
  object PackedDouble extends ValType[Double] {
    override def alignment: Long = 1L
    override def size: Long = 8L
    override def load(mem: Mem, address: Long): Double =
      mem.loadUnalignedDouble(address)
    override def store(mem: Mem, address: Long, value: Double): Unit =
      mem.storeUnalignedDouble(address, value)
    override def toString: String = "PackedDouble"
  }
  
  object PackedBoolean extends ValType[Boolean] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(mem: Mem, address: Long): Boolean =
      mem.loadByte(address) == 0
    override def store(mem: Mem, address: Long, value: Boolean): Unit =
      mem.storeByte(address, if (value) 0.toByte else -1.toByte)
    override def toString: String = "PackedBoolean"
  }
  
  object PaddedShort extends ValType[Short] {
    override def alignment: Long = 2L
    override def size: Long = 2L
    override def load(mem: Mem, address: Long): Short =
      mem.loadShort(address)
    override def store(mem: Mem, address: Long, value: Short): Unit =
      mem.storeShort(address, value)
    override def toString: String = "PaddedShort"
  }
  
  object PaddedInt extends ValType[Int] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(mem: Mem, address: Long): Int =
      mem.loadInt(address)
    override def store(mem: Mem, address: Long, value: Int): Unit =
      mem.storeInt(address, value)
    override def toString: String = "PaddedInt"
  }
  
  object PaddedLong extends ValType[Long] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(mem: Mem, address: Long): Long =
      mem.loadLong(address)
    override def store(mem: Mem, address: Long, value: Long): Unit =
      mem.storeLong(address, value)
    override def toString: String = "PaddedLong"
  }
  
  object PaddedFloat extends ValType[Float] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(mem: Mem, address: Long): Float =
      mem.loadFloat(address)
    override def store(mem: Mem, address: Long, value: Float): Unit =
      mem.storeFloat(address, value)
    override def toString: String = "PaddedFloat"
  }
  
  object PaddedDouble extends ValType[Double] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(mem: Mem, address: Long): Double =
      mem.loadDouble(address)
    override def store(mem: Mem, address: Long, value: Double): Unit =
      mem.storeDouble(address, value)
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
    override def load(mem: Mem, address: Long): (T1, T2) =
      (field1.load(mem, address),
       field2.load(mem, address + offset2))
    override def store(mem: Mem, address: Long, tuple: (T1, T2)) {
      field1.store(mem, address,           tuple._1)
      field2.store(mem, address + offset2, tuple._2)
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
    override def load(mem: Mem, address: Long): (T1, T2, T3) =
      (field1.load(mem, address),
       field2.load(mem, address + offset2),
       field3.load(mem, address + offset3))
    override def store(mem: Mem, address: Long, tuple: (T1, T2, T3)) {
      field1.store(mem, address,           tuple._1)
      field2.store(mem, address + offset2, tuple._2)
      field3.store(mem, address + offset3, tuple._3)
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
    override def load(mem: Mem, address: Long): (T1, T2, T3, T4) =
      (field1.load(mem, address),
       field2.load(mem, address + offset2),
       field3.load(mem, address + offset3),
       field4.load(mem, address + offset4))
    override def store(mem: Mem, address: Long, tuple: (T1, T2, T3, T4)) {
      field1.store(mem, address,           tuple._1)
      field2.store(mem, address + offset2, tuple._2)
      field3.store(mem, address + offset3, tuple._3)
      field4.store(mem, address + offset4, tuple._4)
    }
    override def toString: String =
      "Record4"+"("+ field1 +", "+ field2 +", "+ field3 +", "+ field4 +")"
  }
}

private[data] class RefTypes {
  implicit def Reference[T]: RefType[T] = RefType[T]
}

private[data] class ValTypes extends RefTypes {
  implicit def PackedByte: ValType.PackedByte.type = ValType.PackedByte
  def PackedShort: ValType.PackedShort.type = ValType.PackedShort
  def PackedInt: ValType.PackedInt.type = ValType.PackedInt
  def PackedLong: ValType.PackedLong.type = ValType.PackedLong
  def PackedFloat: ValType.PackedFloat.type = ValType.PackedFloat
  def PackedDouble: ValType.PackedDouble.type = ValType.PackedDouble
  implicit def PackedBoolean: ValType.PackedBoolean.type = ValType.PackedBoolean
  implicit def PaddedShort: ValType.PaddedShort.type = ValType.PaddedShort
  implicit def PaddedInt: ValType.PaddedInt.type = ValType.PaddedInt
  implicit def PaddedLong: ValType.PaddedLong.type = ValType.PaddedLong
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
