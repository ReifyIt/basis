/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.unicode

/** A builder for Unicode® 8-bit strings in the UTF-8 encoding form. Produces
  * only well-formed code unit sequences.
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a builder with a buffer for at least `sizeHint` 8-bit code units.
  */
final class U8StringBuilder(sizeHint: Int) extends UStringBuilder[U8String] {
  /** Constructs a builder with an unallocated buffer. */
  def this() = this(0)
  
  /** The number of 8-bit code units so far appended to this builder. */
  private[this] var length: Int = 0
  
  /** The buffer of 8-bit code units comprising the appended content.  */
  private[this] var codeUnits: Array[Byte] = if (sizeHint > 0) new Array[Byte](sizeHint) else null
  
  override def expect(count: Int): this.type = {
    val size = if (codeUnits != null) codeUnits.length else 0
    if (count > size) {
      // copy any content to a newly allocated, exactly sized buffer
      val newCodeUnits = new Array[Byte](count)
      if (codeUnits != null) Array.copy(codeUnits, 0, newCodeUnits, 0, length)
      codeUnits = newCodeUnits
    }
    this
  }
  
  override def += (codePoint: Int): this.type = {
    val n = length
    if (codePoint >= 0x0000 && codePoint <= 0x007F) {
      // U+0000..U+007F; encode a single byte
      val cs = ensureCapacity(n + 1)
      cs(n) = codePoint.toByte
      length = n + 1
    }
    else if (codePoint >= 0x0080 && codePoint <= 0x07FF) {
      // U+0080..U+07FF; encode 2 bytes
      val cs = ensureCapacity(n + 2)
      cs(n)     = (0xC0 | (codePoint >>> 6)).toByte
      cs(n + 1) = (0x80 | (codePoint & 0x3F)).toByte
      length = n + 2
    }
    else if ((codePoint >= 0x0800 && codePoint <= 0xD7FF) ||
             (codePoint >= 0xE000 && codePoint <= 0xFFFF)) {
      // U+0800..U+D7FF | U+E000..U+FFFF; exclude surrogates, encode 3 bytes
      val cs = ensureCapacity(n + 3)
      cs(n)     = (0xE0 | (codePoint >>> 12)).toByte
      cs(n + 1) = (0x80 | ((codePoint >>> 6) & 0x3F)).toByte
      cs(n + 2) = (0x80 | (codePoint & 0x3F)).toByte
      length = n + 3
    }
    else if (codePoint >= 0x10000 && codePoint <= 0x10FFFF) {
      // U+10000..U+10FFFF; encode 4 bytes
      val cs = ensureCapacity(n + 4)
      cs(n)     = (0xF0 | (codePoint >>> 18)).toByte
      cs(n + 1) = (0x80 | ((codePoint >>> 12) & 0x3F)).toByte
      cs(n + 2) = (0x80 | ((codePoint >>> 6) & 0x3F)).toByte
      cs(n + 3) = (0x80 | (codePoint & 0x3F)).toByte
      length = n + 4
    }
    else {
      // surrogate, encode the replacement character U+FFFD
      val cs = ensureCapacity(n + 3)
      cs(n)     = 0xEF.toByte
      cs(n + 1) = 0xBF.toByte
      cs(n + 2) = 0xBD.toByte
      length = n + 3
    }
    this
  }
  
  override def result: U8String = {
    // reference the buffer and its content length locally
    val newLength = length
    val newCodeUnits = codeUnits
    // clear this builder's buffer reference and zero its size
    length = 0
    codeUnits = null
    if (newLength == 0) U8String.empty // alias the empty string
    else if (newLength == newCodeUnits.length) new U8String(newCodeUnits)
    else {
      // copy content to an exactly sized buffer
      val minCodeUnits = new Array[Byte](newLength)
      Array.copy(newCodeUnits, 0, minCodeUnits, 0, newLength)
      new U8String(minCodeUnits)
    }
  }
  
  override def clear() {
    if (codeUnits != null) length = 0 // result was never called so don't discard the buffer
  }
  
  /** Guarantees that `codeUnits` references a buffer with at least `capacity`
    * elements. Always allocates power-of-2-sized buffers. */
  private def ensureCapacity(capacity: Int): Array[Byte] = {
    val size = if (codeUnits != null) codeUnits.length else 0
    if (capacity > size) {
      // compute a power of 2 at least the required capacity and minimally 16
      var n = math.max(16, capacity) - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n += 1
      // copy any content to a resized buffer
      val newCodeUnits = new Array[Byte](n)
      if (codeUnits != null) Array.copy(codeUnits, 0, newCodeUnits, 0, length)
      codeUnits = newCodeUnits
    }
    codeUnits
  }
}
