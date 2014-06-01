//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

final class DataFactoryOps[+Data](val __ : DataFactory[Data]) extends AnyVal {
  def fromBase16(base16: CharSequence): Data = {
    var i = 0
    val n = base16.length
    if ((n & 1) != 0) throw new IllegalArgumentException
    val framer = __.Framer.expect(n >> 1)
    def decodeDigit(c: Char): Int = {
      if (c >= '0' && c <= '9') c - '0'
      else if (c >= 'A' && c <= 'F') c + (10 - 'A')
      else if (c >= 'a' && c <= 'f') c + (10 - 'a')
      else throw new IllegalArgumentException
    }
    def decodeQuantum(a: Char, b: Char): Unit = {
      val x = decodeDigit(a)
      val y = decodeDigit(b)
      framer.writeByte(((x << 4) | y).toByte)
    }
    while (i + 2 <= n) {
      decodeQuantum(base16.charAt(i), base16.charAt(i + 1))
      i += 2
    }
    framer.state
  }

  def fromBase64(base64: CharSequence): Data = {
    var i = 0
    val n = base64.length
    if ((n & 3) != 0) throw new IllegalArgumentException
    val framer = __.Framer
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
    while (i + 4 <= n) {
      decodeQuantum(base64.charAt(i), base64.charAt(i + 1), base64.charAt(i + 2), base64.charAt(i + 3))
      i += 4
    }
    framer.state
  }
}
