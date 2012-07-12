/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collection._

/** The Unicode® UTF-8 endocing form.
  * 
  * @author Chris Sachs
  */
object UTF8 extends Unicode {
  /** An 8-bit Unicode string comprised of a sequence of UTF-8 code units. */
  final class String(val codeUnits: Array[Byte]) extends AnyVal with Text {
    /** Returns the number of unsigned 8-bit code units in this Unicode string. */
    @inline override def length: Int = codeUnits.length
    
    /** Returns the unsigned 8-bit code unit at the specified index;
      * '''DOES NOT''' decode a character at that index. */
    @inline override def apply(index: Int): Int = codeUnits(index) & 0xFF
    
    /** Updates an unsigned 8-bit code unit of this Unicode string. */
    @inline private[UTF8] def update(index: Int, codeUnit: Int): Unit = codeUnits(index) = codeUnit.toByte
    
    /** Returns a copy of this Unicode string. */
    def copy(length: Int): String = {
      val newCodeUnits = new Array[Byte](length)
      Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, length))
      new String(newCodeUnits)
    }
    
    /** Sequentially applies a function to each code point in this Unicode string.
      * Applies the replacement character U+FFFD in lieu of the maximal subpart of
      * any ill-formed subsequences. */
    override def foreach[@specialized(Unit) U](f: Int => U) {
      var i = 0
      val cs = codeUnits
      val n = cs.length
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
    
    override def iterator: StringIterator = new StringIterator(this, 0)
    
    override def toString: java.lang.String = {
      val s = new java.lang.StringBuilder
      foreach(s.appendCodePoint(_))
      s.toString
    }
  }
  
  /** Contains factory methods for 8-bit Unicode strings. */
  object String extends StringFactory {
    override val Empty = new String(new Array[Byte](0))
    override def apply(chars: CharSequence): String = {
      val s = new StringBuilder
      s.append(chars)
      s.result
    }
  }
  
  /** A pointer to a location in a UTF-8 string. */
  final class StringIterator(val string: String, private[this] var index: Int) extends TextIterator {
    def offset: Int = index
    
    /** Returns `true` if the current offset begins a valid character. */
    def isValid: Boolean = {
      val i = index
      val cs = string.codeUnits
      val n = cs.length
      if (0 <= i && i < n) {
        val c1 = cs(i) & 0xFF
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
      val i = index
      val cs = string.codeUnits
      val n = cs.length
      if (i < 0 || i >= n) throw new NoSuchElementException("head of empty string iterator")
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
    
    override def hasNext: Boolean = 0 <= index && index < string.length
    
    override def next(): Int = {
      var i = index
      val cs = string.codeUnits
      val n = cs.length
      val c = if (0 <= i && i < n) {
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
      }
      else throw new NoSuchElementException("empty string iterator")
      index = i
      c
    }
  }
  
  /** A builder for 8-bit Unicode strings in the UTF-8 encoding form.
    * Produces only well-formed code unit sequences. */
  final class StringBuilder extends super.StringBuilder {
    private[this] var string: String = String.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    
    private[this] def prepare(size: Int): Unit = if (aliased || size > string.length) {
      string = string.copy(Collector.nextSize(16, size))
      aliased = false
    }
    
    override def expect(count: Int): Unit = if (length + count > string.length) {
      string = string.copy(length + count)
      aliased = false
    }
    
    override def += (codePoint: Int) {
      val n = length
      if (codePoint >= 0x0000 && codePoint <= 0x007F) {
        // U+0000..U+007F; encode a single byte
        prepare(n + 1)
        string(n) = codePoint
        length = n + 1
      }
      else if (codePoint >= 0x0080 && codePoint <= 0x07FF) {
        // U+0080..U+07FF; encode 2 bytes
        prepare(n + 2)
        string(n)     = 0xC0 | (codePoint >>> 6)
        string(n + 1) = 0x80 | (codePoint & 0x3F)
        length = n + 2
      }
      else if ((codePoint >= 0x0800 && codePoint <= 0xD7FF) ||
               (codePoint >= 0xE000 && codePoint <= 0xFFFF)) {
        // U+0800..U+D7FF | U+E000..U+FFFF; exclude surrogates, encode 3 bytes
        prepare(n + 3)
        string(n)     = 0xE0 | (codePoint >>> 12)
        string(n + 1) = 0x80 | ((codePoint >>> 6) & 0x3F)
        string(n + 2) = 0x80 | (codePoint & 0x3F)
        length = n + 3
      }
      else if (codePoint >= 0x10000 && codePoint <= 0x10FFFF) {
        // U+10000..U+10FFFF; encode 4 bytes
        prepare(n + 4)
        string(n)     = 0xF0 | (codePoint >>> 18)
        string(n + 1) = 0x80 | ((codePoint >>> 12) & 0x3F)
        string(n + 2) = 0x80 | ((codePoint >>> 6) & 0x3F)
        string(n + 3) = 0x80 | (codePoint & 0x3F)
        length = n + 4
      }
      else {
        // surrogate, encode the replacement character U+FFFD
        prepare(n + 3)
        string(n)     = 0xEF
        string(n + 1) = 0xBF
        string(n + 2) = 0xBD
        length = n + 3
      }
    }
    
    override def result: String = {
      if (length != string.length) string = string.copy(length)
      aliased = true
      string
    }
    
    override def clear() {
      string = String.Empty
      aliased = true
      length = 0
    }
  }
  
  /** Returns a new 8-bit Unicode string builder. */
  implicit def StringBuilder: StringBuilder = new StringBuilder
}
