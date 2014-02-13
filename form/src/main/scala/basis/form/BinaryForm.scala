//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.memory._
import basis.text._
import scala.reflect._

trait BinaryForm { variant: Variant =>
  type BinaryForm <: BaseBinary with AnyForm

  val BinaryForm: BaseBinaryFactory

  implicit def BinaryFormTag: ClassTag[BinaryForm]

  trait BaseBinary extends Equals with Loader with BaseValue { this: BinaryForm =>
    override def isBinaryForm: Boolean = true

    override def asBinaryForm: BinaryForm = this

    def size: Long

    def writeBase64(builder: StringBuilder): Unit = {
      def encodeDigit(digit: Int): Int = {
        if      (digit >=  0 && digit < 26) digit + 'A'
        else if (digit >= 26 && digit < 52) digit + ('a' - 26)
        else if (digit >= 52 && digit < 62) digit + ('0' - 52)
        else if (digit == 62) '+'
        else if (digit == 63) '/'
        else throw new MatchError(digit.toString)
      }
      var i = 0L
      while (canLoad(i + 2L)) {
        val x: Int = loadByte(i) & 0xFF
        val y: Int = loadByte(i + 1L) & 0xFF
        val z: Int = loadByte(i + 2L) & 0xFF
        builder.append(encodeDigit(x >>> 2))
        builder.append(encodeDigit(((x << 4) | (y >>> 4)) & 0x3F))
        builder.append(encodeDigit(((y << 2) | (z >>> 6)) & 0x3F))
        builder.append(encodeDigit(z & 0x3F))
        i += 3L
      }
      if (canLoad(i + 1L)) {
        val x = loadByte(i) & 0xFF
        val y = loadByte(i + 1L) & 0xFF
        builder.append(encodeDigit(x >>> 2))
        builder.append(encodeDigit(((x << 4) | (y >>> 4)) & 0x3F))
        builder.append(encodeDigit((y << 2) & 0x3F))
        builder.append('=')
        i += 2L
      }
      else if (canLoad(i)) {
        val x = loadByte(i) & 0xFF
        builder.append(encodeDigit(x >>> 2))
        builder.append(encodeDigit((x << 4) & 0x3F))
        builder.append('=')
        builder.append('=')
        i += 1L
      }
    }

    def toBase64: String = {
      val s = UString.Builder()
      writeBase64(s)
      s.state.toString
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseBinary]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[BaseBinary] && {
      val that = other.asInstanceOf[BaseBinary]
      val n = size
      var i = 0
      that.canEqual(this) && that.size == n && {
        while (i < n && loadByte(i) == that.loadByte(i)) i += 1
        i == n
      }
    }

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      var h = seed[BinaryForm]
      var i = 0
      val n = size
      while (i < n) {
        h = mix(h, loadByte(i.toLong).toInt)
        i += 1
      }
      mash(h)
    }

    override def toString: String = {
      val s = UString.Builder()
      s.append("BinaryForm")
      s.append('(')
      s.append('\"')
      writeBase64(s)
      s.append('\"')
      s.append(')')
      s.state.toString
    }
  }

  trait BaseBinaryFactory {
    def empty: BinaryForm = Framer().state

    def apply(data: Array[Byte]): BinaryForm = {
      var i = 0
      val n = data.length
      val framer = BinaryForm.Framer().expect(n.toLong)
      while (i < n) {
        framer.writeByte(data(i))
        i += 1
      }
      framer.state
    }

    def apply(base64: CharSequence): BinaryForm = {
      val framer = Framer()
      val n = base64.length
      var i = 0
      def decodeDigit(c: Char): Int = {
        if (c >= 'A' && c <= 'Z') c - 'A'
        else if (c >= 'a' && c <= 'z') c + (26 - 'a')
        else if (c >= '0' && c <= '9') c + (52 - '0')
        else if (c == '+' || c == '-') 62
        else if (c == '/' || c == '_') 63
        else throw new IllegalArgumentException
      }
      def decodeQuantum(a: Char, b: Char, c: Char, d: Char): Unit = {
        val x = decodeDigit(a)
        val y = decodeDigit(b)
        if (c != '=') {
          val z = decodeDigit(c)
          if (d != '=') {
            val w = decodeDigit(d)
            framer.writeByte(((x << 2) | (y >>> 4)).toByte)
            framer.writeByte(((y << 4) | (z >>> 2)).toByte)
            framer.writeByte(((z << 6) | w).toByte)
          }
          else {
            framer.writeByte(((x << 2) | (y >>> 4)).toByte)
            framer.writeByte(((y << 4) | (z >>> 2)).toByte)
          }
        }
        else {
          if (d != '=') throw new IllegalArgumentException
          framer.writeByte(((x << 2) | (y >>> 4)).toByte)
        }
      }
      if (n > 0) while (i + 4 <= n) {
        decodeQuantum(base64.charAt(i), base64.charAt(i + 1), base64.charAt(i + 2), base64.charAt(i + 3))
        i += 4
      }
      if (i != n) throw new IllegalArgumentException
      framer.state
    }

    def Framer(): Framer with State[BinaryForm]

    override def toString: String = "BinaryForm"
  }
}
