//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.text._
import scala.reflect._

trait NumberForm { variant: Variant =>
  type NumberForm <: BaseNumber with AnyForm

  val NumberForm: BaseNumberFactory

  implicit def NumberFormTag: ClassTag[NumberForm]

  trait BaseNumber extends Equals with BaseValue { this: NumberForm =>
    override def isNumberForm: Boolean = true

    override def asNumberForm: NumberForm = this

    def isValidByte: Boolean

    def isValidShort: Boolean

    def isValidInt: Boolean

    def isValidLong: Boolean

    def isValidFloat: Boolean

    def isValidDouble: Boolean

    def isValidDecimal: Boolean

    def toByte: Byte

    def toShort: Short

    def toInt: Int

    def toLong: Long

    def toFloat: Float

    def toDouble: Double

    def toDecimalString: String

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseNumber]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseNumber =>
        that.canEqual(this) &&
        isValidByte && that.isValidByte && toByte == that.toByte ||
        isValidShort && that.isValidShort && toShort == that.toShort ||
        isValidInt && that.isValidInt && toInt == that.toInt ||
        isValidLong && that.isValidLong && toLong == that.toLong ||
        isValidFloat && that.isValidFloat && toFloat == that.toFloat ||
        isValidDouble && that.isValidDouble && toDouble == that.toDouble ||
        toDecimalString.equals(that.toDecimalString)
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      val h =
        if (isValidByte) hash(toByte)
        else if (isValidShort) hash(toShort)
        else if (isValidInt) hash(toInt)
        else if (isValidLong) hash(toLong)
        else if (isValidFloat) hash(toFloat)
        else if (isValidDouble) hash(toDouble)
        else toDecimalString.hashCode
      mash(mix(seed[NumberForm], h))
    }

    override def toString: String = {
      val s = UString.Builder()
      s.append("NumberForm")
      s.append('(')
      s.append(toDecimalString)
      s.append(')')
      s.state.toString
    }
  }

  protected trait BaseInt extends BaseNumber { this: NumberForm =>
    override def isValidByte: Boolean = toByte == toInt
    override def isValidShort: Boolean = toShort == toInt
    override def isValidInt: Boolean = true
    override def isValidLong: Boolean = true
    override def isValidFloat: Boolean = true
    override def isValidDouble: Boolean = true
    override def isValidDecimal: Boolean = true
    override def toByte: Byte = toInt.toByte
    override def toShort: Short = toInt.toShort
    override def toLong: Long = toInt.toLong
    override def toFloat: Float = toInt.toFloat
    override def toDouble: Double = toInt.toDouble
    override def toDecimalString: String = java.lang.Integer.toString(toInt)
  }

  protected trait BaseLong extends BaseNumber { this: NumberForm =>
    override def isValidByte: Boolean = toByte == toLong
    override def isValidShort: Boolean = toShort == toLong
    override def isValidInt: Boolean = toInt == toLong
    override def isValidLong: Boolean = true
    override def isValidFloat: Boolean = true
    override def isValidDouble: Boolean = true
    override def isValidDecimal: Boolean = true
    override def toByte: Byte = toLong.toByte
    override def toShort: Short = toLong.toShort
    override def toInt: Int = toLong.toInt
    override def toFloat: Float = toLong.toFloat
    override def toDouble: Double = toLong.toDouble
    override def toDecimalString: String = java.lang.Long.toString(toLong)
  }

  protected trait BaseFloat extends BaseNumber { this: NumberForm =>
    override def isValidByte: Boolean = toByte == toFloat
    override def isValidShort: Boolean = toShort == toFloat
    override def isValidInt: Boolean = toInt == toFloat
    override def isValidLong: Boolean = toLong == toFloat
    override def isValidFloat: Boolean = true
    override def isValidDouble: Boolean = true
    override def isValidDecimal: Boolean =
      !java.lang.Float.isNaN(toFloat) && !java.lang.Float.isInfinite(toFloat)
    override def toByte: Byte = toFloat.toByte
    override def toShort: Short = toFloat.toShort
    override def toInt: Int = toFloat.toInt
    override def toLong: Long = toFloat.toLong
    override def toDouble: Double = toFloat.toDouble
    override def toDecimalString: String = java.lang.Float.toString(toFloat)
  }

  protected trait BaseDouble extends BaseNumber { this: NumberForm =>
    override def isValidByte: Boolean = toByte == toDouble
    override def isValidShort: Boolean = toShort == toDouble
    override def isValidInt: Boolean = toInt == toDouble
    override def isValidLong: Boolean = toLong == toDouble
    override def isValidFloat: Boolean = toFloat == toDouble
    override def isValidDouble: Boolean = true
    override def isValidDecimal: Boolean =
      !java.lang.Double.isNaN(toDouble) && !java.lang.Double.isInfinite(toDouble)
    override def toByte: Byte = toDouble.toByte
    override def toShort: Short = toDouble.toShort
    override def toInt: Int = toDouble.toInt
    override def toLong: Long = toDouble.toLong
    override def toFloat: Float = toDouble.toFloat
    override def toDecimalString: String = java.lang.Double.toString(toDouble)
  }

  protected trait BaseDecimalString extends BaseNumber { this: NumberForm =>
    override def isValidByte: Boolean =
      try { java.lang.Byte.parseByte(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def isValidShort: Boolean =
      try { java.lang.Short.parseShort(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def isValidInt: Boolean =
      try { java.lang.Integer.parseInt(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def isValidLong: Boolean =
      try { java.lang.Long.parseLong(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def isValidFloat: Boolean =
      try { java.lang.Float.parseFloat(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def isValidDouble: Boolean =
      try { java.lang.Double.parseDouble(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def isValidDecimal: Boolean =
      try { new java.math.BigDecimal(toDecimalString); true }
      catch { case _: NumberFormatException => false }
    override def toByte: Byte =
      try java.lang.Byte.parseByte(toDecimalString)
      catch {
        case _: NumberFormatException =>
          new java.math.BigDecimal(toDecimalString).byteValue
      }
    override def toShort: Short =
      try java.lang.Short.parseShort(toDecimalString)
      catch {
        case _: NumberFormatException =>
          new java.math.BigDecimal(toDecimalString).shortValue
      }
    override def toInt: Int =
      try java.lang.Integer.parseInt(toDecimalString)
      catch {
        case _: NumberFormatException =>
          new java.math.BigDecimal(toDecimalString).intValue
      }
    override def toLong: Long =
      try java.lang.Long.parseLong(toDecimalString)
      catch {
        case _: NumberFormatException =>
          new java.math.BigDecimal(toDecimalString).longValue
      }
    override def toFloat: Float =
      try java.lang.Float.parseFloat(toDecimalString)
      catch {
        case _: NumberFormatException =>
          new java.math.BigDecimal(toDecimalString).floatValue
      }
    override def toDouble: Double =
      try java.lang.Double.parseDouble(toDecimalString)
      catch {
        case _: NumberFormatException =>
          new java.math.BigDecimal(toDecimalString).doubleValue
      }
  }

  trait BaseNumberFactory {
    def apply(value: Byte): NumberForm

    def apply(value: Short): NumberForm

    def apply(value: Int): NumberForm

    def apply(value: Long): NumberForm

    def apply(value: Float): NumberForm

    def apply(value: Double): NumberForm

    def apply(value: String): NumberForm

    override def toString: String = "NumberForm"
  }
}
