/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.unicode

/** A Unicode® 8-bit string comprised of a sequence of UTF-8 code units.
  * 
  * @author Chris Sachs
  */
final class U8String(val codeUnits: Array[Byte]) extends AnyVal with UString {
  /** Returns the number of unsigned 8-bit code units in this Unicode string. */
  override def length: Int = codeUnits.length
  
  /** Returns the unsigned 8-bit code unit at the specified index;
    * '''DOES NOT''' decode a character at that index. */
  override def apply(index: Int): Int = codeUnits(index) & 0xFF
  
  /** Sequentially applies a function to each code point of this Unicode string.
    * Applies the replacement character U+FFFD in lieu of the maximal subpart of
    * any ill-formed subsequences. */
  override def foreach[@specialized(Unit) U](f: Int => U) {
    val cs = codeUnits
    val n = cs.length
    var i = 0
    while (i < n) f({
      val c1 = cs(i) & 0xFF
      if (c1 <= 0x7F) {
        // U+0000..U+007F
        i += 1 // valid 1st code unit
        c1
      }
      else if (c1 >= 0xC2 && c1 <= 0xDF) {
        // U+0080..U+07FF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0x80 && c2 <= 0xBF) {
            i += 1 // valid 2nd code unit
            ((c1 & 0x1F) << 6) | (c2 & 0x3F)
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 == 0xE0) {
        // U+0800..U+0FFF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0xA0 && c2 <= 0xBF) {
            i += 1 // valid 2nd code unit
            if (i < n) {
              val c3 = cs(i) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                i += 1 // valid 3rd code unit
                ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
              }
              else 0xFFFD // invalid 3rd code unit
            }
            else 0xFFFD // missing 3rd code unit
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 == 0xED) {
        // U+D000..U+D7FF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0x80 && c2 <= 0x9F) {
            i += 1 // valid 2nd code unit
            if (i < n) {
              val c3 = cs(i) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                i += 1 // valid 3rd code unit
                ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
              }
              else 0xFFFD // invalid 3rd code unit
            }
            else 0xFFFD // missing 3rd code unit
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 >= 0xE1 && c1 <= 0xEF) { // c1 != 0xED
        // U+1000..U+CFFF | U+E000..U+FFFF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0x80 && c2 <= 0xBF) {
            i += 1 // valid 2nd code unit
            if (i < n) {
              val c3 = cs(i) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                i += 1 // valid 3rd code unit
                ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
              }
              else 0xFFFD // invalid 3rd code unit
            }
            else 0xFFFD // missing 3rd code unit
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 == 0xF0) {
        // U+10000..U+3FFFF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0x90 && c2 <= 0xBF) {
            i += 1 // valid 2nd code unit
            if (i < n) {
              val c3 = cs(i) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                i += 1 // valid 3rd code unit
                if (i < n) {
                  val c4 = cs(i) & 0xFF
                  if (c4 >= 0x80 && c4 <= 0xBF) {
                    i += 1 // valid 4th code unit
                    ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
                  }
                  else 0xFFFD // invalid 4th code unit
                }
                else 0xFFFD // missing 4th code unit
              }
              else 0xFFFD // invalid 3rd code unit
            }
            else 0xFFFD // missing 3rd code unit
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 >= 0xF1 && c1 <= 0xF3) {
        // U+40000..U+FFFFF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0x80 && c2 <= 0xBF) {
            i += 1 // valid 2nd code unit
            if (i < n) {
              val c3 = cs(i) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                i += 1 // valid 3rd code unit
                if (i < n) {
                  val c4 = cs(i) & 0xFF
                  if (c4 >= 0x80 && c4 <= 0xBF) {
                    i += 1 // valid 4th code unit
                    ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
                  }
                  else 0xFFFD // invalid 4th code unit
                }
                else 0xFFFD // missing 4th code unit
              }
              else 0xFFFD // invalid 3rd code unit
            }
            else 0xFFFD // missing 3rd code unit
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 == 0xF4) {
        // U+100000..U+10FFFF
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = cs(i) & 0xFF
          if (c2 >= 0x80 && c2 <= 0x8F) {
            i += 1 // valid 2nd code unit
            if (i < n) {
              val c3 = cs(i) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                i += 1 // valid 3rd code unit
                if (i < n) {
                  val c4 = cs(i) & 0xFF
                  if (c4 >= 0x80 && c4 <= 0xBF) {
                    i += 1 // valid 4th code unit
                    ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
                  }
                  else 0xFFFD // invalid 4th code unit
                }
                else 0xFFFD // missing 4th code unit
              }
              else 0xFFFD // invalid 3rd code unit
            }
            else 0xFFFD // missing 3rd code unit
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else {
        i += 1
        0xFFFD // invalid 1st code unit
      }
    }: Int) // ascribe Int to defer boxing for unspecialized functions
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    foreach(s.appendCodePoint(_))
    s.toString
  }
  
  override def iterator: U8StringCursor = new U8StringCursor(this, 0)
}

/** Factory methods for Unicode® 8-bit strings. */
object U8String {
  /** Returns the empty Unicode 8-bit string. */
  val empty: U8String = new U8String(new Array[Byte](0))
  
  /** Returns a new Unicode 8-bit string comprised of the valid code points
    * in the given UTF-16 character sequence. Substitutes the replacement
    * character U+FFFD for any unpaired surrogates. */
  def apply(chars: CharSequence): U8String =
    new U8StringBuilder(chars.length).append(chars).result
}
