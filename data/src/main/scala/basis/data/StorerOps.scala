//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.reflect.macros._

final class StorerOps(val __ : Storer) extends AnyVal {
  import __._

  def store[T](address: Long, value: T)(implicit T: Struct[T]): Unit                                     = macro StorerMacros.store[T]
  def storeArray[T](address: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = macro StorerMacros.storeArray[T]

  def storeShortBE(address: Long, value: Short): Unit = {
    if (endian.isBig) storeShort(address, value)
    else if (endian.isLittle) {
      storeByte(address     , (value     ).toByte)
      storeByte(address + 1L, (value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }

  def storeShortLE(address: Long, value: Short): Unit = {
    if (endian.isLittle) storeShort(address, value)
    else if (endian.isBig) {
      storeByte(address     , (value >> 8).toByte)
      storeByte(address + 1L, (value     ).toByte)
    }
    else throw new MatchError(endian)
  }

  def storeIntBE(address: Long, value: Int): Unit = {
    if (endian.isBig) storeInt(address, value)
    else if (endian.isLittle) {
      storeByte(address     , (value      ).toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }

  def storeIntLE(address: Long, value: Int): Unit = {
    if (endian.isLittle) storeInt(address, value)
    else if (endian.isBig) {
      storeByte(address     , (value >> 24).toByte)
      storeByte(address + 1L, (value >> 16).toByte)
      storeByte(address + 2L, (value >>  8).toByte)
      storeByte(address + 3L, (value      ).toByte)
    }
    else throw new MatchError(endian)
  }

  def storeLongBE(address: Long, value: Long): Unit = {
    if (endian.isBig) storeLong(address, value)
    else if (endian.isLittle) {
      storeByte(address     , (value      ).toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
      storeByte(address + 4L, (value >> 32).toByte)
      storeByte(address + 5L, (value >> 40).toByte)
      storeByte(address + 6L, (value >> 48).toByte)
      storeByte(address + 7L, (value >> 56).toByte)
    }
    else throw new MatchError(endian)
  }

  def storeLongLE(address: Long, value: Long): Unit = {
    if (endian.isLittle) storeLong(address, value)
    else if (endian.isBig) {
      storeByte(address     , (value >> 56).toByte)
      storeByte(address + 1L, (value >> 48).toByte)
      storeByte(address + 2L, (value >> 40).toByte)
      storeByte(address + 3L, (value >> 32).toByte)
      storeByte(address + 4L, (value >> 24).toByte)
      storeByte(address + 5L, (value >> 16).toByte)
      storeByte(address + 6L, (value >>  8).toByte)
      storeByte(address + 7L, (value      ).toByte)
    }
    else throw new MatchError(endian)
  }

  def storeFloatBE(address: Long, value: Float): Unit = {
    if (endian.isBig) storeFloat(address, value)
    else if (endian.isLittle) storeIntBE(address, value.toRawIntBits)
    else throw new MatchError(endian)
  }

  def storeFloatLE(address: Long, value: Float): Unit = {
    if (endian.isLittle) storeFloat(address, value)
    else if (endian.isBig) storeIntLE(address, value.toRawIntBits)
    else throw new MatchError(endian)
  }

  def storeDoubleBE(address: Long, value: Double): Unit = {
    if (endian.isBig) storeDouble(address, value)
    else if (endian.isLittle) storeLongBE(address, value.toRawLongBits)
    else throw new MatchError(endian)
  }

  def storeDoubleLE(address: Long, value: Double): Unit = {
    if (endian.isLittle) storeDouble(address, value)
    else if (endian.isBig) storeLongLE(address, value.toRawLongBits)
    else throw new MatchError(endian)
  }
}

private[data] class StorerMacros(val c: blackbox.Context { type PrefixType <: StorerOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def store[T](address: Expr[Long], value: Expr[T])(T: Expr[Struct[T]]): Expr[Unit] =  Expr[Unit](q"$T.store($prefix.__, $address, $value)")

  def storeArray[T](address: Expr[Long], array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Struct[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
    val T = $T
    val xs = $array
    var p = $address
    var i = $start
    val n = i + $count
    while (i < n) {
      T.store(data, p, xs(i))
      p += T.size
      i += 1
    }
  }""")
}
