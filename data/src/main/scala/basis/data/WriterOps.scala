//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.reflect.macros._

final class WriterOps(val __ : Writer) extends AnyVal {
  import __._

  def write[T](value: T)(implicit T: Frame[T]): Unit                                     = macro WriterMacros.write[T]
  def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Frame[T]): Unit = macro WriterMacros.writeArray[T]

  def writeShortBE(value: Short): Unit = {
    if (endian.isBig) writeShort(value)
    else if (endian.isLittle) {
      writeByte((value     ).toByte)
      writeByte((value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeShortLE(value: Short): Unit = {
    if (endian.isLittle) writeShort(value)
    else if (endian.isBig) {
      writeByte((value >> 8).toByte)
      writeByte((value     ).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeIntBE(value: Int): Unit = {
    if (endian.isBig) writeInt(value)
    else if (endian.isLittle) {
      writeByte((value      ).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeIntLE(value: Int): Unit = {
    if (endian.isLittle) writeInt(value)
    else if (endian.isBig) {
      writeByte((value >> 24).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value      ).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeLongBE(value: Long): Unit = {
    if (endian.isBig) writeLong(value)
    else if (endian.isLittle) {
      writeByte((value      ).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
      writeByte((value >> 32).toByte)
      writeByte((value >> 40).toByte)
      writeByte((value >> 48).toByte)
      writeByte((value >> 56).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeLongLE(value: Long): Unit = {
    if (endian.isLittle) writeLong(value)
    else if (endian.isBig) {
      writeByte((value >> 56).toByte)
      writeByte((value >> 48).toByte)
      writeByte((value >> 40).toByte)
      writeByte((value >> 32).toByte)
      writeByte((value >> 24).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value      ).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeFloatBE(value: Float): Unit = {
    if (endian.isBig) writeFloat(value)
    else if (endian.isLittle) writeIntBE(value.toRawIntBits)
    else throw new MatchError(endian)
  }

  def writeFloatLE(value: Float): Unit = {
    if (endian.isLittle) writeFloat(value)
    else if (endian.isBig) writeIntLE(value.toRawIntBits)
    else throw new MatchError(endian)
  }

  def writeDoubleBE(value: Double): Unit = {
    if (endian.isBig) writeDouble(value)
    else if (endian.isLittle) writeLongBE(value.toRawLongBits)
    else throw new MatchError(endian)
  }

  def writeDoubleLE(value: Double): Unit = {
    if (endian.isLittle) writeDouble(value)
    else if (endian.isBig) writeLongLE(value.toRawLongBits)
    else throw new MatchError(endian)
  }
}

private[data] class WriterMacros(val c: blackbox.Context { type PrefixType <: WriterOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def write[T](value: Expr[T])(T: Expr[Frame[T]]): Expr[Unit] = Expr[Unit](q"$T.write($prefix.__, $value)")

  def writeArray[T](array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Frame[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
    val T = $T
    val xs = $array
    var i = $start
    val n = i + $count
    while (i < n) {
      T.write(data, xs(i))
      i += 1
    }
  }""")
}
