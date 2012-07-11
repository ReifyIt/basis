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
object UTF8 extends Encoding {
  /** A Unicode 8-bit string comprised of a sequence of UTF-8 code units. */
  final class String(val codeUnits: Array[Byte]) extends AnyVal with Text {
    @inline override def length: Int = codeUnits.length
    @inline override def apply(index: Int): Int = codeUnits(index) & 0xFF
    @inline private[UTF8] def update(index: Int, codeUnit: Int): Unit = codeUnits(index) = codeUnit.toByte
    def copy(length: Int): String = {
      val newCodeUnits = new Array[Byte](length)
      Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, length))
      new String(newCodeUnits)
    }
  }
  
  /** Contains factory methods for Unicode 8-bit strings. */
  object String extends StringFactory {
    override val Empty = new String(new Array[Byte](0))
    override def apply(chars: CharSequence): String = {
      val s = new StringBuilder
      s.append(chars)
      s.result
    }
  }
  
  /** Returns a new Unicode 8-bit string builder. */
  implicit def StringBuilder: StringBuilder = new StringBuilder
  
  /** A UTF-8 code unit sequence. */
  trait Text extends Any with super.Text {
    /** Returns the number of unsigned 8-bit code units in this Unicode string. */
    override def length: Int
    
    /** Returns the unsigned 8-bit code unit at the specified index;
      * '''DOES NOT''' decode a character at that index. */
    override def apply(index: Int): Int
    
    /** Sequentially applies a function to each code point of this Unicode string.
      * Applies the replacement character U+FFFD in lieu of the maximal subpart of
      * any ill-formed subsequences. */
    override def foreach[@specialized(Unit) U](f: Int => U) {
      val n = length
      var i = 0
      while (i < n) f({
        val c1 = apply(i)
        if (c1 <= 0x7F) {
          // U+0000..U+007F
          i += 1 // valid 1st code unit
          c1
        }
        else if (c1 >= 0xC2 && c1 <= 0xDF) {
          // U+0080..U+07FF
          i += 1 // valid 1st code unit
          if (i < n) {
            val c2 = apply(i)
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
            val c2 = apply(i)
            if (c2 >= 0xA0 && c2 <= 0xBF) {
              i += 1 // valid 2nd code unit
              if (i < n) {
                val c3 = apply(i)
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
            val c2 = apply(i)
            if (c2 >= 0x80 && c2 <= 0x9F) {
              i += 1 // valid 2nd code unit
              if (i < n) {
                val c3 = apply(i)
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
            val c2 = apply(i)
            if (c2 >= 0x80 && c2 <= 0xBF) {
              i += 1 // valid 2nd code unit
              if (i < n) {
                val c3 = apply(i)
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
            val c2 = apply(i)
            if (c2 >= 0x90 && c2 <= 0xBF) {
              i += 1 // valid 2nd code unit
              if (i < n) {
                val c3 = apply(i)
                if (c3 >= 0x80 && c3 <= 0xBF) {
                  i += 1 // valid 3rd code unit
                  if (i < n) {
                    val c4 = apply(i)
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
            val c2 = apply(i)
            if (c2 >= 0x80 && c2 <= 0xBF) {
              i += 1 // valid 2nd code unit
              if (i < n) {
                val c3 = apply(i)
                if (c3 >= 0x80 && c3 <= 0xBF) {
                  i += 1 // valid 3rd code unit
                  if (i < n) {
                    val c4 = apply(i)
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
            val c2 = apply(i)
            if (c2 >= 0x80 && c2 <= 0x8F) {
              i += 1 // valid 2nd code unit
              if (i < n) {
                val c3 = apply(i)
                if (c3 >= 0x80 && c3 <= 0xBF) {
                  i += 1 // valid 3rd code unit
                  if (i < n) {
                    val c4 = apply(i)
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
    
    override def iterator: TextIterator = new TextIterator(this, 0)
    
    override def toString: java.lang.String = {
      val s = new java.lang.StringBuilder
      foreach(s.appendCodePoint(_))
      s.toString
    }
  }
  
  /** A pointer to a location in some UTF-8 text. */
  class TextIterator(string: Text, private[this] var index: Int) extends super.TextIterator {
    def offset: Int = index
    
    override def hasNext: Boolean = {
      val n = string.length
      val i = index
      if (0 <= i && i < n) {
        val c1 = string(index)
        if (c1 <= 0x7F) true // U+0000..U+007F
        else if (i + 1 < n && c1 >= 0xC2 && c1 <= 0xDF) {
          // U+0080..U+07FF
          val c2 = string(i + 1)
          c2 >= 0x80 && c2 <= 0xBF
        }
        else if (i + 2 < n && c1 == 0xE0) {
          // U+0800..U+0FFF
          val c2 = string(i + 1)
          val c3 = string(i + 2)
          c2 >= 0xA0 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF
        }
        else if (i + 2 < n && c1 == 0xED) {
          // U+D000..U+D7FF
          val c2 = string(i + 1)
          val c3 = string(i + 2)
          c2 >= 0x80 && c2 <= 0x9F &&
          c3 >= 0x80 && c3 <= 0xBF
        }
        else if (i + 2 < n && c1 >= 0xE1 && c1 <= 0xEF) { // c1 != 0xED
          // U+1000..U+CFFF | U+E000..U+FFFF
          val c2 = string(i + 1)
          val c3 = string(i + 2)
          c2 >= 0x80 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF
        }
        else if (i + 3 < n && c1 == 0xF0) {
          // U+10000..U+3FFFF
          val c2 = string(i + 1)
          val c3 = string(i + 2)
          val c4 = string(i + 3)
          c2 >= 0x90 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF &&
          c4 >= 0x80 && c4 <= 0xBF
        }
        else if (i + 3 < n && c1 >= 0xF1 && c1 <= 0xF3) {
          // U+40000..U+FFFFF
          val c2 = string(i + 1)
          val c3 = string(i + 2)
          val c4 = string(i + 3)
          c2 >= 0x80 && c2 <= 0xBF &&
          c3 >= 0x80 && c3 <= 0xBF &&
          c4 >= 0x80 && c4 <= 0xBF
        }
        else if (i + 3 < n && c1 == 0xF4) {
          // U+100000..U+10FFFF
          val c2 = string(i + 1)
          val c3 = string(i + 2)
          val c4 = string(i + 3)
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
      val n = string.length
      val i = index
      if (i < 0 || i >= n) throw new IndexOutOfBoundsException(i.toString)
      val c1 = string(i)
      if (c1 <= 0x7F) c1 // U+0000..U+007F
      else if (i + 1 < n && c1 >= 0xC2 && c1 <= 0xDF) {
        // U+0080..U+07FF
        val c2 = string(i + 1)
        if (c2 >= 0x80 && c2 <= 0xBF)
          ((c1 & 0x1F) << 6) | (c2 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else if (i + 2 < n && c1 == 0xE0) {
        // U+0800..U+0FFF
        val c2 = string(i + 1)
        val c3 = string(i + 2)
        if (c2 >= 0xA0 && c2 <= 0xBF &&
            c3 >= 0x80 && c3 <= 0xBF)
          ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else if (i + 2 < n && c1 == 0xED) {
        // U+D000..U+D7FF
        val c2 = string(i + 1)
        val c3 = string(i + 2)
        if (c2 >= 0x80 && c2 <= 0x9F &&
            c3 >= 0x80 && c3 <= 0xBF)
          ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else if (i + 2 < n && c1 >= 0xE1 && c1 <= 0xEF) { // c1 != 0xED
        // U+1000..U+CFFF | U+E000..U+FFFF
        val c2 = string(i + 1)
        val c3 = string(i + 2)
        if (c2 >= 0x80 && c2 <= 0xBF &&
            c3 >= 0x80 && c3 <= 0xBF)
          ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else if (i + 3 < n && c1 == 0xF0) {
        // U+10000..U+3FFFF
        val c2 = string(i + 1)
        val c3 = string(i + 2)
        val c4 = string(i + 3)
        if (c2 >= 0x90 && c2 <= 0xBF &&
            c3 >= 0x80 && c3 <= 0xBF &&
            c4 >= 0x80 && c4 <= 0xBF)
          ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else if (i + 3 < n && c1 >= 0xF1 && c1 <= 0xF3) {
        // U+40000..U+FFFFF
        val c2 = string(i + 1)
        val c3 = string(i + 2)
        val c4 = string(i + 3)
        if (c2 >= 0x80 && c2 <= 0xBF &&
            c3 >= 0x80 && c3 <= 0xBF &&
            c4 >= 0x80 && c4 <= 0xBF)
          ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else if (i + 3 < n && c1 == 0xF4) {
        // U+100000..U+10FFFF
        val c2 = string(i + 1)
        val c3 = string(i + 2)
        val c4 = string(i + 3)
        if (c2 >= 0x80 && c2 <= 0x8F &&
            c3 >= 0x80 && c3 <= 0xBF &&
            c4 >= 0x80 && c4 <= 0xBF)
          ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
        else 0xFFFD // unconvertible offset
      }
      else 0xFFFD // unconvertible offset
    }
    
    override def next(): Int = {
      val n = string.length
      if (0 <= index && index < n) {
        val c1 = string(index)
        if (c1 <= 0x7F) {
          // U+0000..U+007F
          index += 1 // valid 1st code unit
          c1
        }
        else if (c1 >= 0xC2 && c1 <= 0xDF) {
          // U+0080..U+07FF
          index += 1 // valid 1st code unit
          if (index < n) {
            val c2 = string(index)
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
            val c2 = string(index)
            if (c2 >= 0xA0 && c2 <= 0xBF) {
              index += 1 // valid 2nd code unit
              if (index < n) {
                val c3 = string(index)
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
            val c2 = string(index)
            if (c2 >= 0x80 && c2 <= 0x9F) {
              index += 1 // valid 2nd code unit
              if (index < n) {
                val c3 = string(index)
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
            val c2 = string(index)
            if (c2 >= 0x80 && c2 <= 0xBF) {
              index += 1 // valid 2nd code unit
              if (index < n) {
                val c3 = string(index)
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
            val c2 = string(index)
            if (c2 >= 0x90 && c2 <= 0xBF) {
              index += 1 // valid 2nd code unit
              if (index < n) {
                val c3 = string(index)
                if (c3 >= 0x80 && c3 <= 0xBF) {
                  index += 1 // valid 3rd code unit
                  if (index < n) {
                    val c4 = string(index)
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
            val c2 = string(index)
            if (c2 >= 0x80 && c2 <= 0xBF) {
              index += 1 // valid 2nd code unit
              if (index < n) {
                val c3 = string(index)
                if (c3 >= 0x80 && c3 <= 0xBF) {
                  index += 1 // valid 3rd code unit
                  if (index < n) {
                    val c4 = string(index)
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
            val c2 = string(index)
            if (c2 >= 0x80 && c2 <= 0x8F) {
              index += 1 // valid 2nd code unit
              if (index < n) {
                val c3 = string(index)
                if (c3 >= 0x80 && c3 <= 0xBF) {
                  index += 1 // valid 3rd code unit
                  if (index < n) {
                    val c4 = string(index)
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
      else Iterator.Empty.next() // end of sequence
    }
  }
  
  /** A builder for Unicode 8-bit strings in the UTF-8 encoding form.
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
        string(n) = codePoint.toByte
        length = n + 1
      }
      else if (codePoint >= 0x0080 && codePoint <= 0x07FF) {
        // U+0080..U+07FF; encode 2 bytes
        prepare(n + 2)
        string(n)     = (0xC0 | (codePoint >>> 6)).toByte
        string(n + 1) = (0x80 | (codePoint & 0x3F)).toByte
        length = n + 2
      }
      else if ((codePoint >= 0x0800 && codePoint <= 0xD7FF) ||
               (codePoint >= 0xE000 && codePoint <= 0xFFFF)) {
        // U+0800..U+D7FF | U+E000..U+FFFF; exclude surrogates, encode 3 bytes
        prepare(n + 3)
        string(n)     = (0xE0 | (codePoint >>> 12)).toByte
        string(n + 1) = (0x80 | ((codePoint >>> 6) & 0x3F)).toByte
        string(n + 2) = (0x80 | (codePoint & 0x3F)).toByte
        length = n + 3
      }
      else if (codePoint >= 0x10000 && codePoint <= 0x10FFFF) {
        // U+10000..U+10FFFF; encode 4 bytes
        prepare(n + 4)
        string(n)     = (0xF0 | (codePoint >>> 18)).toByte
        string(n + 1) = (0x80 | ((codePoint >>> 12) & 0x3F)).toByte
        string(n + 2) = (0x80 | ((codePoint >>> 6) & 0x3F)).toByte
        string(n + 3) = (0x80 | (codePoint & 0x3F)).toByte
        length = n + 4
      }
      else {
        // surrogate, encode the replacement character U+FFFD
        prepare(n + 3)
        string(n)     = 0xEF.toByte
        string(n + 1) = 0xBF.toByte
        string(n + 2) = 0xBD.toByte
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
}
