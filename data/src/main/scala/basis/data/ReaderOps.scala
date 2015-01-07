//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.reflect.macros._

final class ReaderOps(val __ : Reader) extends AnyVal {
  import __._

  def read[T]()(implicit T: Frame[T]): T                                                  = macro ReaderMacros.read[T]
  def readArray[T](count: Int)(implicit T: Frame[T]): Array[T]                            = macro ReaderMacros.readArray[T]
  def readToArray[T](array: Array[T], start: Int, count: Int)(implicit T: Frame[T]): Unit = macro ReaderMacros.readToArray[T]

  def readShortBE(): Short = {
    if (endian.isBig) readShort()
    else if (endian.isLittle)
      ((readByte() & 0xFF)     ) |
      ((readByte()       ) << 8)
    else throw new MatchError(endian)
  }.toShort

  def readShortLE(): Short = {
    if (endian.isLittle) readShort()
    else if (endian.isBig)
      ((readByte()       ) << 8) |
      ((readByte() & 0xFF)     )
    else throw new MatchError(endian)
  }.toShort

  def readIntBE(): Int = {
    if (endian.isBig) readInt()
    else if (endian.isLittle)
      ((readByte() & 0xFF)      ) |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF) << 16) |
      ((readByte()       ) << 24)
    else throw new MatchError(endian)
  }

  def readIntLE(): Int = {
    if (endian.isLittle) readInt()
    else if (endian.isBig)
      ((readByte()       ) << 24) |
      ((readByte() & 0xFF) << 16) |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF)      )
    else throw new MatchError(endian)
  }

  def readLongBE(): Long = {
    if (endian.isBig) readLong()
    else if (endian.isLittle)
      ((readByte() & 0xFF).toLong      ) |
      ((readByte() & 0xFF).toLong <<  8) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 48) |
      ((readByte()       ).toLong << 56)
    else throw new MatchError(endian)
  }

  def readLongLE(): Long = {
    if (endian.isLittle) readLong()
    else if (endian.isBig)
      ((readByte()       ).toLong << 56) |
      ((readByte() & 0xFF).toLong << 48) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong <<  8) |
      ((readByte() & 0xFF).toLong      )
    else throw new MatchError(endian)
  }

  def readFloatBE(): Float = {
    if (endian.isBig) readFloat()
    else if (endian.isLittle) readIntBE().toFloatBits
    else throw new MatchError(endian)
  }

  def readFloatLE(): Float = {
    if (endian.isLittle) readFloat()
    else if (endian.isBig) readIntLE().toFloatBits
    else throw new MatchError(endian)
  }

  def readDoubleBE(): Double = {
    if (endian.isBig) readDouble()
    else if (endian.isLittle) readLongBE().toDoubleBits
    else throw new MatchError(endian)
  }

  def readDoubleLE(): Double = {
    if (endian.isLittle) readDouble()
    else if (endian.isBig) readLongLE().toDoubleBits
    else throw new MatchError(endian)
  }
}

private[data] class ReaderMacros(val c: blackbox.Context { type PrefixType <: ReaderOps }) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def read[T]()(T: Expr[Frame[T]])(implicit TTag: WeakTypeTag[T]): Expr[T] = Expr[T](q"$T.read($prefix.__)")

  def readArray[T](count: Expr[T])(T: Expr[Frame[T]])(implicit TTag: WeakTypeTag[T]): Expr[Array[T]] = Expr[Array[T]](q"""{
    val data = $prefix.__
    val T = $T
    val xs = new Array[$TTag]($count)
    var i = 0
    while (i < count) {
      xs(i) = T.read(data)
      i += 1
    }
    xs
  }""")

  def readToArray[T](array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Frame[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
    val T = $T
    val xs = $array
    var i = $start
    val n = i + $count
    while (i < n) {
      xs(i) = T.read(data)
      i += 1
    }
  }""")

  implicit protected def ArrayTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Array[A]] =
    WeakTypeTag(appliedType(definitions.ArrayClass.toTypeConstructor, A.tpe :: Nil))
}
