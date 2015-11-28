//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis.collections._
import basis.text._
import basis.util._

class Uri private[net] (
    val scheme: Scheme,
    val authority: Authority,
    val path: Path,
    val query: Query,
    val fragment: Fragment) {

  def isDefined: Boolean =
    scheme.isDefined || authority.isDefined ||
    path.isDefined || query.isDefined || fragment.isDefined

  def resolve(relative: Uri): Uri = {
    if (relative.scheme.isDefined)
      new Uri(
        relative.scheme,
        relative.authority,
        relative.path.removeDotSegments,
        relative.query,
        relative.fragment)
    else if (relative.authority.isDefined)
      new Uri(
        scheme,
        relative.authority,
        relative.path.removeDotSegments,
        relative.query,
        relative.fragment)
    else if (relative.path.isEmpty)
      new Uri(
        scheme,
        authority,
        path,
        if (relative.query.isDefined) relative.query else query,
        relative.fragment)
    else if (relative.path.head.equals("/"))
      new Uri(
        scheme,
        authority,
        relative.path.removeDotSegments,
        relative.query,
        relative.fragment)
    else
      new Uri(
        scheme,
        authority,
        merge(relative.path).removeDotSegments,
        relative.query,
        relative.fragment)
  }

  private def merge(relative: Path): Path = {
    if (authority.isDefined && path.isEmpty) "/" :: relative
    else if (path.isEmpty) relative
    else path.merge(relative)
  }

  def unresolve(that: Uri): Uri = {
    if (!scheme.equals(that.scheme) || !authority.equals(that.authority)) that
    else
      new Uri(
        Scheme.Undefined,
        Authority.Undefined,
        path.unmerge(that.path),
        that.query,
        that.fragment)
  }

  def writeUriString(builder: Builder[Int]): Unit = {
    if (scheme.isDefined) {
      scheme.writeUriString(builder)
      builder.append(':')
    }
    if (authority.isDefined) {
      builder.append('/')
      builder.append('/')
      authority.writeUriString(builder)
    }
    path.writeUriString(builder)
    if (query.isDefined) {
      builder.append('?')
      query.writeUriString(builder)
    }
    if (fragment.isDefined) {
      builder.append('#')
      fragment.writeUriString(builder)
    }
    builder.state
  }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  def copy(
      scheme: Scheme = this.scheme,
      authority: Authority = this.authority,
      path: Path = this.path,
      query: Query = this.query,
      fragment: Fragment = this.fragment)
    : Uri =
    new Uri(scheme, authority, path, query, fragment)

  override def equals(other: Any): Boolean = {
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Uri] && {
      val that = other.asInstanceOf[Uri]
      scheme.equals(that.scheme) && authority.equals(that.authority) &&
      path.equals(that.path) && query.equals(that.query) && fragment.equals(that.fragment)
    }
  }

  private[this] var code: Int = 0
  override def hashCode: Int = {
    import MurmurHash3._
    if (code == 0)
      code =
        mash(mix(mix(mix(mix(mix(seed[Uri],
          scheme.hashCode), authority.hashCode),
          path.hashCode), query.hashCode), fragment.hashCode))
    code
  }

  override def toString: String = {
    val s = String.Builder~"Uri"
    if (!isDefined) s~'.'~"empty"
    else {
      s~'('~'"'
      writeUriString(s)
      s~'"'~')'
    }
    s.state
  }
}

object Uri extends UriParser {
  override type Uri         = basis.net.Uri
  override type Part        = basis.net.UriPart
  override type Scheme      = basis.net.Scheme
  override type Authority   = basis.net.Authority
  override type UserInfo    = basis.net.UserInfo
  override type Host        = basis.net.Host
  override type Port        = basis.net.Port
  override type Path        = basis.net.Path
  override type Query       = basis.net.Query
  override type Fragment    = basis.net.Fragment
  override type PathSegment = String
  override type QueryParam  = (String, String)

  override val Scheme    = basis.net.Scheme
  override val Authority = basis.net.Authority
  override val UserInfo  = basis.net.UserInfo
  override val Host      = basis.net.Host
  override val Port      = basis.net.Port
  override val Path      = basis.net.Path
  override val Query     = basis.net.Query
  override val Fragment  = basis.net.Fragment

  val empty: Uri = apply()

  override def apply(
      scheme: Scheme = basis.net.Scheme.Undefined,
      authority: Authority = basis.net.Authority.Undefined,
      path: Path = basis.net.Path.Empty,
      query: Query = basis.net.Query.Undefined,
      fragment: Fragment = basis.net.Fragment.Undefined)
    : Uri =
    new Uri(scheme, authority, path, query, fragment)

  implicit def apply(uri: String): Uri = {
    val input = new UString(uri).iterator
    var result = UriParser.run(input)
    if (!input.isEmpty)
      result = error(input, expected = "valid URI character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  private[this] def encodeHex(x: Int): Int = if (x < 10) '0' + x else 'A' + (x - 10)
  private[this] def writePctEncoded(c: Int)(builder: Builder[Int]): Unit = {
    builder.append('%')
    builder.append(encodeHex(c >>> 4 & 0xF))
    builder.append(encodeHex(c       & 0xF))
  }
  private[this] def writeEncoded(c: Int)(builder: Builder[Int]): Unit = {
    if (c == 0x00) { // modified UTF-8
      writePctEncoded(0xC0)(builder)
      writePctEncoded(0x80)(builder)
    }
    else if (c >= 0x00 && c <= 0x7F) { // U+0000..U+007F
      writePctEncoded(c)(builder)
    }
    else if (c >= 0x80 && c <= 0x07FF) { // U+0080..U+07FF
      writePctEncoded(0xC0 | (c >>> 6))(builder)
      writePctEncoded(0x80 | (c & 0x3F))(builder)
    }
    else if (c >= 0x0800 && c <= 0xFFFF || // U+0800..U+D7FF
             c >= 0xE000 && c <= 0xFFFF) { // U+E000..U+FFFF
      writePctEncoded(0xE0 | (c >>> 12))(builder)
      writePctEncoded(0x80 | (c >>>  6 & 0x3F))(builder)
      writePctEncoded(0x80 | (c        & 0x3F))(builder)
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      writePctEncoded(0xF0 | (c >>> 18))(builder)
      writePctEncoded(0x80 | (c >>> 12 & 0x3F))(builder)
      writePctEncoded(0x80 | (c >>>  6 & 0x3F))(builder)
      writePctEncoded(0x80 | (c        & 0x3F))(builder)
    }
    else { // surrogate or invalid code point
      writePctEncoded(0xEF)(builder)
      writePctEncoded(0xBF)(builder)
      writePctEncoded(0xBD)(builder)
    }
  }

  private[net] def writeScheme(scheme: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = scheme.length
    while (i < n) {
      val c = scheme.codePointAt(i)
      if (isSchemeChar(c)) builder.append(c)
      else throw new UriException((String.Builder~"Invalid scheme: "~>scheme).state)
      i = scheme.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writeUserInfo(userInfo: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = userInfo.length
    while (i < n) {
      val c = userInfo.codePointAt(i)
      if (isUserInfoChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = userInfo.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writeUser(user: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = user.length
    while (i < n) {
      val c = user.codePointAt(i)
      if (isUserChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = user.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writeHost(address: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = address.length
    while (i < n) {
      val c = address.codePointAt(i)
      if (isHostChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = address.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writePort(number: Int)(builder: Builder[Int]): Unit = {
    var n = number
    var i = 9
    val digits = new Array[Int](10)
    while (n > 0) {
      digits(i) = n % 10
      n /= 10
      i -= 1
    }
    i += 1
    while (i < 10) {
      builder.append('0' + digits(i))
      i += 1
    }
  }

  private[net] def writePathSegment(segment: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = segment.length
    while (i < n) {
      val c = segment.codePointAt(i)
      if (isPathChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = segment.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writeQuery(query: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = query.length
    while (i < n) {
      val c = query.codePointAt(i)
      if (isQueryChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = query.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writeParam(param: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = param.length
    while (i < n) {
      val c = param.codePointAt(i)
      if (isParamChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = param.offsetByCodePoints(i, 1)
    }
  }

  private[net] def writeFragment(fragment: String)(builder: Builder[Int]): Unit = {
    var i = 0
    val n = fragment.length
    while (i < n) {
      val c = fragment.codePointAt(i)
      if (isFragmentChar(c)) builder.append(c)
      else writeEncoded(c)(builder)
      i = fragment.offsetByCodePoints(i, 1)
    }
  }

  override def toString: String = "Uri"
}
