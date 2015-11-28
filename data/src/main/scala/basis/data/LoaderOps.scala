//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.collections._
import basis.text._
import basis.util._
import scala.reflect.macros._

final class LoaderOps[-Family](val __ : Loader) extends AnyVal {
  import __.{ Family => _, _ }

  def load[T](address: Long)(implicit T: Struct[T]): T                                                    = macro LoaderMacros.load[T]
  def loadArray[T](address: Long, count: Int)(implicit T: Struct[T]): Array[T]                            = macro LoaderMacros.loadArray[T]
  def loadToArray[T](address: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = macro LoaderMacros.loadToArray[T]

  def loadShortBE(address: Long): Short = {
    if (endian.isBig) loadShort(address)
    else if (endian.isLittle)
      ((loadByte(address     ) & 0xFF)     ) |
      ((loadByte(address + 1L)       ) << 8)
    else throw new MatchError(endian)
  }.toShort

  def loadShortLE(address: Long): Short = {
    if (endian.isLittle) loadShort(address)
    else if (endian.isBig)
      ((loadByte(address     )       ) << 8) |
      ((loadByte(address + 1L) & 0xFF)     )
    else throw new MatchError(endian)
  }.toShort

  def loadIntBE(address: Long): Int = {
    if (endian.isBig) loadInt(address)
    else if (endian.isLittle)
      ((loadByte(address     ) & 0xFF)      ) |
      ((loadByte(address + 1L) & 0xFF) <<  8) |
      ((loadByte(address + 2L) & 0xFF) << 16) |
      ((loadByte(address + 3L)       ) << 24)
    else throw new MatchError(endian)
  }

  def loadIntLE(address: Long): Int = {
    if (endian.isLittle) loadInt(address)
    else if (endian.isBig)
      ((loadByte(address     )       ) << 24) |
      ((loadByte(address + 1L) & 0xFF) << 16) |
      ((loadByte(address + 2L) & 0xFF) <<  8) |
      ((loadByte(address + 3L) & 0xFF)      )
    else throw new MatchError(endian)
  }

  def loadLongBE(address: Long): Long = {
    if (endian.isBig) loadLong(address)
    else if (endian.isLittle)
      ((loadByte(address     ) & 0xFF).toLong      ) |
      ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 2L) & 0xFF).toLong << 16) |
      ((loadByte(address + 3L) & 0xFF).toLong << 24) |
      ((loadByte(address + 4L) & 0xFF).toLong << 32) |
      ((loadByte(address + 5L) & 0xFF).toLong << 40) |
      ((loadByte(address + 6L) & 0xFF).toLong << 48) |
      ((loadByte(address + 7L)       ).toLong << 56)
    else throw new MatchError(endian)
  }

  def loadLongLE(address: Long): Long = {
    if (endian.isLittle) loadLong(address)
    else if (endian.isBig)
      ((loadByte(address     )       ).toLong << 56) |
      ((loadByte(address + 1L) & 0xFF).toLong << 48) |
      ((loadByte(address + 2L) & 0xFF).toLong << 40) |
      ((loadByte(address + 3L) & 0xFF).toLong << 32) |
      ((loadByte(address + 4L) & 0xFF).toLong << 24) |
      ((loadByte(address + 5L) & 0xFF).toLong << 16) |
      ((loadByte(address + 6L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 7L) & 0xFF).toLong      )
    else throw new MatchError(endian)
  }

  def loadFloatBE(address: Long): Float = {
    if (endian.isBig) loadFloat(address)
    else if (endian.isLittle) loadIntBE(address).toFloatBits
    else throw new MatchError(endian)
  }

  def loadFloatLE(address: Long): Float = {
    if (endian.isLittle) loadFloat(address)
    else if (endian.isBig) loadIntLE(address).toFloatBits
    else throw new MatchError(endian)
  }

  def loadDoubleBE(address: Long): Double = {
    if (endian.isBig) loadDouble(address)
    else if (endian.isLittle) loadLongBE(address).toDoubleBits
    else throw new MatchError(endian)
  }

  def loadDoubleLE(address: Long): Double = {
    if (endian.isLittle) loadDouble(address)
    else if (endian.isBig) loadLongLE(address).toDoubleBits
    else throw new MatchError(endian)
  }

  def ++ (that: Loader)(implicit framer: Framer with From[Family]): framer.State = {
    framer.writeData(__)
    framer.writeData(that)
    framer.state
  }

  def writeBase16(builder: Builder[Int]): Unit = {
    def encodeDigit(digit: Int): Int = {
      if      (digit >=  0 && digit < 10) digit + ('0'     )
      else if (digit >= 10 && digit < 16) digit + ('A' - 10)
      else throw new MatchError(digit.toString)
    }
    var i = 0L
    val n = size
    while (i < n) {
      val x = loadByte(i) & 0xFF
      builder.append(encodeDigit(x >>> 4))
      builder.append(encodeDigit(x & 0xF))
      i += 1L
    }
  }

  def writeBase64(builder: Builder[Int]): Unit = {
    def encodeDigit(digit: Int): Int = {
      if      (digit >=  0 && digit < 26) digit + ('A'     )
      else if (digit >= 26 && digit < 52) digit + ('a' - 26)
      else if (digit >= 52 && digit < 62) digit + ('0' - 52)
      else if (digit == 62) '+'
      else if (digit == 63) '/'
      else throw new MatchError(digit.toString)
    }
    var i = 0L
    val n = size
    while (i + 2L < n) {
      val x = loadByte(i     ) & 0xFF
      val y = loadByte(i + 1L) & 0xFF
      val z = loadByte(i + 2L) & 0xFF
      builder.append(encodeDigit(x >>> 2))
      builder.append(encodeDigit(((x << 4) | (y >>> 4)) & 0x3F))
      builder.append(encodeDigit(((y << 2) | (z >>> 6)) & 0x3F))
      builder.append(encodeDigit(z & 0x3F))
      i += 3L
    }
    if (i + 1L < n) {
      val x = loadByte(i     ) & 0xFF
      val y = loadByte(i + 1L) & 0xFF
      builder.append(encodeDigit(x >>> 2))
      builder.append(encodeDigit(((x << 4) | (y >>> 4)) & 0x3F))
      builder.append(encodeDigit((y << 2) & 0x3F))
      builder.append('=')
      i += 2L
    }
    else if (i < n) {
      val x = loadByte(i) & 0xFF
      builder.append(encodeDigit(x >>> 2))
      builder.append(encodeDigit((x << 4) & 0x3F))
      builder.append('=')
      builder.append('=')
      i += 1L
    }
  }

  def toBase16: String = {
    val s = String.Builder
    writeBase16(s)
    s.state
  }

  def toBase64: String = {
    val s = String.Builder
    writeBase64(s)
    s.state
  }
}

private[data] class LoaderMacros(val c: blackbox.Context { type PrefixType <: LoaderOps[_] }) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def load[T](address: Expr[Long])(T: Expr[Struct[T]])(implicit TTag: WeakTypeTag[T]): Expr[T] = Expr[T](q"$T.load($prefix.__, $address)")

  def loadArray[T](address: Expr[Long], count: Expr[Int])(T: Expr[Struct[T]])(implicit TTag: WeakTypeTag[T]): Expr[Array[T]] = Expr[Array[T]](q"""{
    val data = $prefix.__
    val T = $T
    val xs = new Array[$TTag]($count)
    var p = $address
    var i = 0
    while (i < $count) {
      xs(i) = T.load(data, p)
      p += T.size
      i += 1
    }
    xs
  }""")

  def loadToArray[T](address: Expr[Long], array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Struct[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
    val T = $T
    val xs = $array
    var p = $address
    var i = $start
    val n = i + $count
    while (i < n) {
      xs(i) = T.load(data, p)
      p += T.size
      i += 1
    }
  }""")

  implicit protected def ArrayTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Array[A]] =
    WeakTypeTag(appliedType(definitions.ArrayClass.toTypeConstructor, A.tpe :: Nil))
}
