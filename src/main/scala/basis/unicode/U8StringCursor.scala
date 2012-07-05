/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.unicode

/** A pointer to a location in a Uniocde® 8-bit string.
  * 
  * @author Chris Sachs
  */
final class U8StringCursor(val string: U8String, private[this] var index: Int) extends UStringCursor {
  def offset: Int = index
  
  override def hasNext: Boolean = {
    val cs = string.codeUnits
    val n = cs.length
    val i = index
    if (0 <= i && i < n) {
      val c1 = cs(index) & 0xFF
      if (c1 <= 0x7F) true // U+0000..U+007F
      else if (i + 1 < n && c1 >= 0xC2 && c1 <= 0xDF) {
        // U+0080..U+07FF
        val c2 = cs(i + 1) & 0xFF
        c2 >= 0x80 && c2 <= 0xBF
      }
      else if (i + 2 < n && c1 == 0xE0) {
        // U+0800..U+0FFF
        val c2 = cs(i + 1) & 0xFF
        val c3 = cs(i + 2) & 0xFF
        c2 >= 0xA0 && c2 <= 0xBF &&
        c3 >= 0x80 && c3 <= 0xBF
      }
      else if (i + 2 < n && c1 == 0xED) {
        // U+D000..U+D7FF
        val c2 = cs(i + 1) & 0xFF
        val c3 = cs(i + 2) & 0xFF
        c2 >= 0x80 && c2 <= 0x9F &&
        c3 >= 0x80 && c3 <= 0xBF
      }
      else if (i + 2 < n && c1 >= 0xE1 && c1 <= 0xEF) { // c1 != 0xED
        // U+1000..U+CFFF | U+E000..U+FFFF
        val c2 = cs(i + 1) & 0xFF
        val c3 = cs(i + 2) & 0xFF
        c2 >= 0x80 && c2 <= 0xBF &&
        c3 >= 0x80 && c3 <= 0xBF
      }
      else if (i + 3 < n && c1 == 0xF0) {
        // U+10000..U+3FFFF
        val c2 = cs(i + 1) & 0xFF
        val c3 = cs(i + 2) & 0xFF
        val c4 = cs(i + 3) & 0xFF
        c2 >= 0x90 && c2 <= 0xBF &&
        c3 >= 0x80 && c3 <= 0xBF &&
        c4 >= 0x80 && c4 <= 0xBF
      }
      else if (i + 3 < n && c1 >= 0xF1 && c1 <= 0xF3) {
        // U+40000..U+FFFFF
        val c2 = cs(i + 1) & 0xFF
        val c3 = cs(i + 2) & 0xFF
        val c4 = cs(i + 3) & 0xFF
        c2 >= 0x80 && c2 <= 0xBF &&
        c3 >= 0x80 && c3 <= 0xBF &&
        c4 >= 0x80 && c4 <= 0xBF
      }
      else if (i + 3 < n && c1 == 0xF4) {
        // U+100000..U+10FFFF
        val c2 = cs(i + 1) & 0xFF
        val c3 = cs(i + 2) & 0xFF
        val c4 = cs(i + 3) & 0xFF
        c2 >= 0x80 && c2 <= 0x8F &&
        c3 >= 0x80 && c3 <= 0xBF &&
        c4 >= 0x80 && c4 <= 0xBF
      }
      else false // unconvertible offset
    }
    else false // out of bounds
  }
  
  /** Decodes the character at the current offset, substituting the
    * replacement character U+FFFD if the offset is unconvertible. */
  def head: Int = {
    val cs = string.codeUnits
    val n = cs.length
    val i = index
    if (i < 0 || i >= n) throw new IndexOutOfBoundsException(i.toString)
    val c1 = cs(i) & 0xFF
    if (c1 <= 0x7F) c1 // U+0000..U+007F
    else if (i + 1 < n && c1 >= 0xC2 && c1 <= 0xDF) {
      // U+0080..U+07FF
      val c2 = cs(i + 1) & 0xFF
      if (c2 >= 0x80 && c2 <= 0xBF)
        ((c1 & 0x1F) << 6) | (c2 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else if (i + 2 < n && c1 == 0xE0) {
      // U+0800..U+0FFF
      val c2 = cs(i + 1) & 0xFF
      val c3 = cs(i + 2) & 0xFF
      if (c2 >= 0xA0 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF)
        ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else if (i + 2 < n && c1 == 0xED) {
      // U+D000..U+D7FF
      val c2 = cs(i + 1) & 0xFF
      val c3 = cs(i + 2) & 0xFF
      if (c2 >= 0x80 && c2 <= 0x9F &&
          c3 >= 0x80 && c3 <= 0xBF)
        ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else if (i + 2 < n && c1 >= 0xE1 && c1 <= 0xEF) { // c1 != 0xED
      // U+1000..U+CFFF | U+E000..U+FFFF
      val c2 = cs(i + 1) & 0xFF
      val c3 = cs(i + 2) & 0xFF
      if (c2 >= 0x80 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF)
        ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else if (i + 3 < n && c1 == 0xF0) {
      // U+10000..U+3FFFF
      val c2 = cs(i + 1) & 0xFF
      val c3 = cs(i + 2) & 0xFF
      val c4 = cs(i + 3) & 0xFF
      if (c2 >= 0x90 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF &&
          c4 >= 0x80 && c4 <= 0xBF)
        ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else if (i + 3 < n && c1 >= 0xF1 && c1 <= 0xF3) {
      // U+40000..U+FFFFF
      val c2 = cs(i + 1) & 0xFF
      val c3 = cs(i + 2) & 0xFF
      val c4 = cs(i + 3) & 0xFF
      if (c2 >= 0x80 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF &&
          c4 >= 0x80 && c4 <= 0xBF)
        ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else if (i + 3 < n && c1 == 0xF4) {
      // U+100000..U+10FFFF
      val c2 = cs(i + 1) & 0xFF
      val c3 = cs(i + 2) & 0xFF
      val c4 = cs(i + 3) & 0xFF
      if (c2 >= 0x80 && c2 <= 0x8F &&
          c3 >= 0x80 && c3 <= 0xBF &&
          c4 >= 0x80 && c4 <= 0xBF)
        ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
      else 0xFFFD // unconvertible offset
    }
    else 0xFFFD // unconvertible offset
  }
  
  override def next(): Int = {
    val cs = string.codeUnits
    val n = cs.length
    if (0 <= index && index < n) {
      val c1 = cs(index) & 0xFF
      if (c1 <= 0x7F) {
        // U+0000..U+007F
        index += 1 // valid 1st code unit
        c1
      }
      else if (c1 >= 0xC2 && c1 <= 0xDF) {
        // U+0080..U+07FF
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0x80 && c2 <= 0xBF) {
            index += 1 // valid 2nd code unit
            ((c1 & 0x1F) << 6) | (c2 & 0x3F)
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else if (c1 == 0xE0) {
        // U+0800..U+0FFF
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0xA0 && c2 <= 0xBF) {
            index += 1 // valid 2nd code unit
            if (index < n) {
              val c3 = cs(index) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                index += 1 // valid 3rd code unit
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
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0x80 && c2 <= 0x9F) {
            index += 1 // valid 2nd code unit
            if (index < n) {
              val c3 = cs(index) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                index += 1 // valid 3rd code unit
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
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0x80 && c2 <= 0xBF) {
            index += 1 // valid 2nd code unit
            if (index < n) {
              val c3 = cs(index) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                index += 1 // valid 3rd code unit
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
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0x90 && c2 <= 0xBF) {
            index += 1 // valid 2nd code unit
            if (index < n) {
              val c3 = cs(index) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                index += 1 // valid 3rd code unit
                if (index < n) {
                  val c4 = cs(index) & 0xFF
                  if (c4 >= 0x80 && c4 <= 0xBF) {
                    index += 1 // valid 4th code unit
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
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0x80 && c2 <= 0xBF) {
            index += 1 // valid 2nd code unit
            if (index < n) {
              val c3 = cs(index) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                index += 1 // valid 3rd code unit
                if (index < n) {
                  val c4 = cs(index) & 0xFF
                  if (c4 >= 0x80 && c4 <= 0xBF) {
                    index += 1 // valid 4th code unit
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
        index += 1 // valid 1st code unit
        if (index < n) {
          val c2 = cs(index) & 0xFF
          if (c2 >= 0x80 && c2 <= 0x8F) {
            index += 1 // valid 2nd code unit
            if (index < n) {
              val c3 = cs(index) & 0xFF
              if (c3 >= 0x80 && c3 <= 0xBF) {
                index += 1 // valid 3rd code unit
                if (index < n) {
                  val c4 = cs(index) & 0xFF
                  if (c4 >= 0x80 && c4 <= 0xBF) {
                    index += 1 // valid 4th code unit
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
        index += 1
        0xFFFD // invalid 1st code unit
      }
    }
    else -1 // end of sequence
  }
}
