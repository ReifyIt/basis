//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.collections._
import basis.text._
import basis.util._

class Uri(
    val scheme: String,
    val authority: Authority,
    val path: Path,
    val query: Maybe[String],
    val fragment: Maybe[String]) {

  def isEmpty: Boolean =
    scheme.length == 0 && authority.isEmpty &&
    path.length == 0 && query.isEmpty && fragment.isEmpty

  def writeUriString(builder: StringBuilder): Unit = {
    if (scheme.length > 0 || !authority.isEmpty) {
      Uri.writeEncoded(scheme)
      builder.append(':')
      if (!authority.isEmpty) {
        builder.append('/')
        builder.append('/')
        authority.writeUriString(builder)
      }
    }
    var path = this.path
    while (!path.isEmpty) {
      Uri.writeEncoded(path.head)
      path = path.tail
    }
    if (query.canBind) {
      builder.append('?')
      Uri.writeEncoded(query.bind)
    }
    if (fragment.canBind) {
      builder.append('#')
      Uri.writeEncoded(fragment.bind)
    }
    builder.state
  }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def equals(other: Any): Boolean = other match {
    case that: Uri =>
      scheme.equals(that.scheme) && authority.equals(that.authority) &&
      path.equals(that.path) && query.equals(that.query) && fragment.equals(that.fragment)
    case _ => false
  }

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(mix(mix(mix(mix(seed[Uri],
      scheme.hashCode), authority.hashCode),
      path.hashCode), query.hashCode), fragment.hashCode))
  }

  override def toString: String = {
    var i = 0
    val s = String.Builder~"Uri"~'('
    if (scheme.length > 0) {
      s~"scheme"~" = "~>scheme
      i += 1
    }
    if (!authority.isEmpty) {
      if (i > 0) s~", "
      s~"authority"~" = "~>authority
      i += 1
    }
    if (!path.isEmpty) {
      if (i > 0) s~", "
      s~"path"~" = "~>path
      i += 1
    }
    if (query.canBind) {
      if (i > 0) s~", "
      s~"query"~" = "~>query
      i += 1
    }
    if (fragment.canBind) {
      if (i > 0) s~", "
      s~"fragment"~" = "~>fragment
    }
    (s~')').state
  }
}

object Uri {
  val empty: Uri = new Uri("", Authority.empty, Path.empty, Trap, Trap)

  def apply(
      scheme: String = "",
      authority: Authority = Authority.empty,
      path: Path = Path.Empty,
      query: Maybe[String] = Trap,
      fragment: Maybe[String] = Trap)
    : Uri =
    new Uri(scheme, authority, path, query, fragment)

  def isUnreservedChar(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '-' || c == '.' ||
    c == '_' || c == '~'

  def isSubDelimChar(c: Int): Boolean =
    c == '!' || c == '$' ||
    c == '&' || c == '\'' ||
    c == '(' || c == ')' ||
    c == '*' || c == '+' ||
    c == ',' || c == ';' ||
    c == '='

  private[this] def hexToChar(x: Int): Int = if (x < 10) '0' + x else 'A' + (x - 10)
  private[this] def writeEscaped(c: Int)(implicit builder: StringBuilder): Unit = {
    builder.append('%')
    builder.append(hexToChar(c >>> 4 & 0xF))
    builder.append(hexToChar(c       & 0xF))
  }
  def writeEncoded(c: Int)(implicit builder: StringBuilder): Unit = {
    if (isUnreservedChar(c)) builder.append(c)
    else if (c == 0x00) { // modified UTF-8
      writeEscaped(0xC0)
      writeEscaped(0x80)
    }
    else if (c >= 0x00 && c <= 0x7F) { // U+0000..U+007F
      writeEscaped(c)
    }
    else if (c >= 0x80 && c <= 0x07FF) { // U+0080..U+07FF
      writeEscaped(0xC0 | (c >>> 6))
      writeEscaped(0x80 | (c & 0x3F))
    }
    else if (c >= 0x0800 && c <= 0xFFFF || // U+0800..U+D7FF
             c >= 0xE000 && c <= 0xFFFF) { // U+E000..U+FFFF
      writeEscaped(0xE0 | (c >>> 12))
      writeEscaped(0x80 | (c >>>  6 & 0x3F))
      writeEscaped(0x80 | (c        & 0x3F))
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      writeEscaped(0xF0 | (c >>> 18))
      writeEscaped(0x80 | (c >>> 12 & 0x3F))
      writeEscaped(0x80 | (c >>>  6 & 0x3F))
      writeEscaped(0x80 | (c        & 0x3F))
    }
    else { // surrogate or invalid code point
      writeEscaped(0xEF)
      writeEscaped(0xBF)
      writeEscaped(0xBD)
    }
  }
  def writeEncoded(s: String)(implicit builder: StringBuilder): Unit = {
    var i = 0
    val n = s.length
    while (i < n) {
      writeEncoded(s.codePointAt(i))
      i = s.offsetByCodePoints(i, 1)
    }
  }

  def encode(s: String): String = {
    val builder = String.Builder
    writeEncoded(s)(builder)
    builder.state
  }

  override def toString: String = "Uri"


  object Parser extends UriParser {
    override type Uri = basis.net.Uri
    override type Scheme = String
    override type Authority = basis.net.Authority
    override type UserInfo = String
    override type Host = basis.net.Host
    override type Port = Int
    override type Path = basis.net.Path
    override type Query = Maybe[String]
    override type Fragment = Maybe[String]

    override def Scheme(scheme: String): String = scheme

    override def Authority(host: Host, port: Port, userInfo: UserInfo): Authority =
      basis.net.Authority(host, port, userInfo)

    override def UserInfo(userInfo: String): String = userInfo

    override def NameHost(address: String): Host = basis.net.Host.Name(address)

    override def IPv4Host(address: String): Host = basis.net.Host.IPv4(address)

    override def IPv6Host(address: String): Host = basis.net.Host.IPv6(address)

    override def EmptyHost: Host = basis.net.Host.empty

    override def Port(port: Int): Int = port

    override def EmptyPath: Path = basis.net.Path.empty

    override def PathBuilder: Builder[String] with State[Path] = basis.net.Path.Builder

    override def toString: String = (String.Builder~"Uri"~'.'~"Parser").state
  }
}
