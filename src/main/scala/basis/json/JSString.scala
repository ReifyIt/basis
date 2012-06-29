/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

final class JSString(val value: String) extends JSValue {
  override protected type Root = JSString
  
  override def write(s: Appendable) {
    def toHexChar(h: Int): Char = (if (h < 10) '0' + h else 'A' + (h - 10)).toChar
    s.append('"')
    var c = '\0'
    var i = 0
    while (i < value.length) {
      val b = c
      c = value.charAt(i)
      c match {
        case '"'  => s.append('\\').append('"')
        case '\\' => s.append('\\').append('\\')
        case '\b' => s.append('\\').append('b')
        case '\f' => s.append('\\').append('f')
        case '\n' => s.append('\\').append('n')
        case '\r' => s.append('\\').append('r')
        case '\t' => s.append('\\').append('t')
        case '/'  if b == '<' => s.append('\\').append('/')
        case c    if (c >= '\u0000' && c < '\u001f') ||
                     (c >= '\u0080' && c < '\u00a0') ||
                     (c >= '\u2000' && c < '\u2100') =>
                     s.append('\\').append('u').
                       append(toHexChar((c >>> 12) & 0xF)).
                       append(toHexChar((c >>>  8) & 0xF)).
                       append(toHexChar((c >>>  4) & 0xF)).
                       append(toHexChar( c         & 0xF))
        case c    => s.append(c)
      }
      i += 1
    }
    s.append('"')
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: JSString => value.equals(that.value)
    case _ => false
  }
  
  override def hashCode: Int = value.hashCode
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    write(s)
    s.toString
  }
}

object JSString {
  lazy val empty: JSString = new JSString("")
  
  def apply(s: String): JSString = new JSString(s)
  
  def unapply(json: JSString): Some[String] = Some(json.value)
  
  object unary_+ extends runtime.AbstractPartialFunction[Any, JSString] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSString]
    override def apply(x: Any): JSString = x.asInstanceOf[JSString]
    override def toString: String = "+JSString"
  }
}
