/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.encoding

final class JString(val unbox: java.lang.String) extends AnyVal with Text {
  override type Kind = JString
  
  @inline override def size: Int = unbox.length
  
  override def apply(index: Int): Int = {
    val n = unbox.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = unbox.charAt(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = unbox.charAt(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
        (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
      else 0xFFFD
    }
    else 0xFFFD
  }
  
  override def advance(index: Int): Int = {
    val n = unbox.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = unbox.charAt(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000)
      index + 1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = unbox.charAt(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) 
        index + 2 // U+10000..U+10FFFF
      else index + 1
    }
    else index + 1
  }
  
  @inline override def foreach[U](f: Int => U) {
    var i = 0
    val n = size
    while (i < n) {
      f(apply(i))
      i = advance(i)
    }
  }
  
  override def iterator: JString.Iterator = new JString.Iterator(this, 0)
  
  override def toString: java.lang.String = unbox
}

object JString {
  val Empty: JString = new JString("")
  
  def apply(chars: CharSequence): JString = {
    val s = new JString.Builder
    s.append(chars)
    s.result
  }
  
  implicit def Builder: JString.Builder = new JString.Builder
  
  final class Iterator(val string: JString, private[this] var index: Int) extends Text.Iterator {
    def offset: Int = index
    
    def isValid: Boolean = {
      val c = string(index)
      c >= 0 && c <= 0x10FFFF
    }
    
    def head: Int = string(index)
    
    override def hasNext: Boolean = 0 <= index && index < string.size
    
    override def next(): Int = {
      val c = string(index)
      index = string.advance(index)
      c
    }
  }
  
  final class Builder extends Text.Builder {
    override type Result = JString
    
    private[this] var builder = new java.lang.StringBuilder
    
    override def expect(count: Int): Unit =
      builder.ensureCapacity(builder.length + count)
    
    override def += (codePoint: Int): Unit =
      builder.appendCodePoint(codePoint)
    
    override def result: JString =
      new JString(builder.toString)
    
    override def clear(): Unit =
      builder.setLength(0)
  }
}
