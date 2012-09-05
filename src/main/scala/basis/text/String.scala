/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

final class String(override val toString: java.lang.String) extends AnyVal with Rope {
  override type Kind = String
  
  @inline override def size: Int = toString.length
  
  override def apply(index: Int): Char = {
    val n = toString.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    new Char {
      val c1 = toString.charAt(index)
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
        val c2 = toString.charAt(index + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        else 0xFFFD
      }
      else 0xFFFD
    }
  }
  
  override def advance(index: Int): Int = {
    val n = toString.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = toString.charAt(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000)
      index + 1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = toString.charAt(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) 
        index + 2 // U+10000..U+10FFFF
      else index + 1
    }
    else index + 1
  }
  
  @inline override def foreach[U](f: Char => U) {
    var i = 0
    val n = size
    while (i < n) {
      f(apply(i))
      i = advance(i)
    }
  }
}

object String {
  val Empty: String = new String("")
  
  def apply(chars: CharSequence): String = {
    val s = new String.Builder
    s.append(chars)
    s.result
  }
  
  implicit def Builder: String.Builder = new String.Builder
  
  final class Builder extends Text.Builder {
    override type Result = String
    
    private[this] var builder = new java.lang.StringBuilder
    
    override def expect(count: Int): Unit =
      builder.ensureCapacity(builder.length + count)
    
    override def += (c: Char): Unit =
      builder.appendCodePoint(c.codePoint)
    
    override def result: String =
      new String(builder.toString)
    
    override def clear(): Unit =
      builder.setLength(0)
  }
}
