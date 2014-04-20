//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

trait ByteFactory[+Data] extends ByteOrder[Endianness] {
  def empty: Data = Framer.state

  def apply(data: Array[Byte]): Data = {
    val size = data.length
    val framer = Framer.expect(size.toLong)
    var i = 0
    while (i < size) {
      framer.writeByte(data(i))
      i += 1
    }
    framer.state
  }

  def apply(base64: CharSequence): Data = {
    val framer = Framer
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

  implicit def Framer: Framer with ByteOrder[Endian] with State[Data]
}
