//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.collections._
import basis.text._

trait UriParser extends UriFactory { Uri =>
  lazy val UriParser: Iteratee[Int, Uri]             = new UriParser()
  lazy val SchemeParser: Iteratee[Int, Scheme]       = new SchemeParser()
  lazy val AuthorityParser: Iteratee[Int, Authority] = new AuthorityParser()
  lazy val UserInfoParser: Iteratee[Int, UserInfo]   = new UserInfoParser()
  lazy val HostParser: Iteratee[Int, Host]           = new HostParser()
  lazy val HostAddressParser: Iteratee[Int, Host]    = new HostAddressParser()
  lazy val HostLiteralParser: Iteratee[Int, Host]    = new HostLiteralParser()
  lazy val PortParser: Iteratee[Int, Port]           = new PortParser()
  lazy val PathParser: Iteratee[Int, Path]           = new PathParser()
  lazy val QueryParser: Iteratee[Int, Query]         = new QueryParser()
  lazy val FragmentParser: Iteratee[Int, Fragment]   = new FragmentParser()


  def isUnreservedChar(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '-' || c == '.' ||
    c == '_' || c == '~'

  def isSubDelimChar(c: Int): Boolean =
    c == '!' || c == '$' ||
    c == '&' || c == '(' ||
    c == ')' || c == '*' ||
    c == '+' || c == ',' ||
    c == ';' || c == '=' ||
    c == '\''

  def isSchemeChar(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '+' || c == '-' ||
    c == '.'

  def isUserInfoChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c) ||
    c == ':'

  def isUserChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c)

  def isHostChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c)

  def isPathChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c) ||
    c == ':' || c == '@'

  def isQueryChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c) ||
    c == '/' || c == ':' ||
    c == '?' || c == '@'

  def isParamChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    c == '!' || c == '$' ||
    c == '(' || c == ')' ||
    c == '*' || c == '+' ||
    c == ',' || c == '/' ||
    c == ':' || c == ';' ||
    c == '?' || c == '@' ||
    c == '\''

  def isFragmentChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c) ||
    c == '/' || c == ':' ||
    c == '?' || c == '@'

  protected def isAlpha(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z'

  protected def isDigit(c: Int): Boolean =
    c >= '0' && c <= '9'

  protected def isHexChar(c: Int): Boolean =
    c >= 'A' && c <= 'F' ||
    c >= 'a' && c <= 'f' ||
    c >= '0' && c <= '9'

  protected def decodeHex(c: Int): Int =
    if (c >= '0' && c <= '9') c - '0'
    else if (c >= 'A' && c <= 'F') 10 + (c - 'A')
    else if (c >= 'a' && c <= 'f') 10 + (c - 'a')
    else throw new AssertionError()

  protected def decodeDigit(c: Int): Int =
    if (c >= '0' && c <= '9') c - '0'
    else throw new AssertionError()


  private[net] abstract class Parser[+O] extends Iteratee[Int, O] {
    def interpolate(part: Part): Iteratee[Int, O] =
      Iteratee.error(new UriException("invalid substitution", Iterator.done))
  }


  private final class UriParser(
      scheme: Iteratee[Int, Scheme],
      authority: Iteratee[Int, Authority],
      path: Iteratee[Int, Path],
      query: Iteratee[Int, Query],
      fragment: Iteratee[Int, Fragment],
      s: Int)
    extends Parser[Uri] {

    def this() = this(SchemeParser, AuthorityParser, PathParser, QueryParser, FragmentParser, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Uri] = {
      var c = 0
      var s = this.s
      var fragment = this.fragment
      var query = this.query
      var path = this.path
      var authority = this.authority
      var scheme = this.scheme
      if (s == 1) {
        if (!input.isEmpty) {
          val look = input.dup
          while (!look.isEmpty && { c = look.head; isSchemeChar(c) }) look.step()
          if (!look.isEmpty && c == ':') s = 2
          else s = 3
        }
        else if (input.isDone) s = 3
      }
      if (s == 2) {
        scheme = scheme.feed(input)
        if (scheme.isError) return scheme.asError
        else if (!input.isEmpty && { c = input.head; c == ':' }) {
          input.step()
          s = 3
        }
        else if (!input.isEmpty) return error(input, expected = ':', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 3) {
        if (!input.isEmpty) {
          c = input.head
          if (c == '/') {
            input.step()
            s = 4
          }
          else if (c == '?') {
            input.step()
            s = 7
          }
          else if (c == '#') {
            input.step()
            s = 8
          }
          else s = 6
        }
        else if (input.isDone) return Iteratee.done(Uri(scheme.bind))
      }
      if (s == 4) {
        if (!input.isEmpty && { c = input.head; c == '/' }) {
          input.step()
          s = 5
        }
        else if (!input.isEmpty) {
          path = new PathParser('/')
          s = 6
        }
        else if (input.isDone) return Iteratee.done(Uri(scheme.bind, path = Path.Slash))
      }
      if (s == 5) {
        authority = authority.feed(input)
        if (authority.isError) return authority.asError
        else if (!input.isEmpty) {
          c = input.head
          if (c == '?') {
            input.step()
            s = 7
          }
          else if (c == '#') {
            input.step()
            s = 8
          }
          else s = 6
        }
        else if (input.isDone) return Iteratee.done(Uri(scheme.bind, authority.bind))
      }
      if (s == 6) {
        path = path.feed(input)
        if (path.isError) return path.asError
        else if (!input.isEmpty) {
          c = input.head
          if (c == '?') {
            input.step()
            s = 7
          }
          else if (c == '#') {
            input.step()
            s = 8
          }
          else return Iteratee.done(Uri(scheme.bind, authority.bind, path.bind))
        }
        else if (input.isDone) return Iteratee.done(Uri(scheme.bind, authority.bind, path.bind))
      }
      if (s == 7) {
        query = query.feed(input)
        if (query.isError) return query.asError
        else if (!input.isEmpty) {
          val c = input.head
          if (c == '#') {
            input.step()
            s = 8
          }
          else return Iteratee.done(Uri(scheme.bind, authority.bind, path.bind, query.bind))
        }
        else if (input.isDone)
          return Iteratee.done(Uri(scheme.bind, authority.bind, path.bind, query.bind))
      }
      if (s == 8) {
        fragment = fragment.feed(input)
        if (fragment.isError) return fragment.asError
        else if (input.isDone)
          return Iteratee.done(Uri(scheme.bind, authority.bind, path.bind, query.bind, fragment.bind))
      }
      new UriParser(scheme, authority, path, query, fragment, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Uri] =
      if (s == 1) part match {
        case Scheme(_) =>
          val scheme = this.scheme.asInstanceOf[Parser[Scheme]].interpolate(part)
          if (scheme.isError) scheme.asError
          else new UriParser(scheme, authority, path, query, fragment, 2)
        case Path(_) =>
          val path = this.path.asInstanceOf[Parser[Path]].interpolate(part)
          if (path.isError) path.asError
          else new UriParser(scheme, authority, path, query, fragment, 6)
      }
      else if (s == 5) part match {
        case Authority(_) =>
          val authority = this.authority.asInstanceOf[Parser[Authority]].interpolate(part)
          if (authority.isError) authority.asError
          else new UriParser(scheme, authority, path, query, fragment, 6)
        case _ =>
          val authority = this.authority.asInstanceOf[Parser[Authority]].interpolate(part)
          if (authority.isError) authority.asError
          else new UriParser(scheme, authority, path, query, fragment, 5)
      }
      else if (s == 4) {
        val path = new PathParser('/').interpolate(part)
        if (path.isError) path.asError
        else new UriParser(scheme, authority, path, query, fragment, 6)
      }
      else if (s == 3 || s == 6) {
        val path = this.path.asInstanceOf[Parser[Path]].interpolate(part)
        if (path.isError) path.asError
        else new UriParser(scheme, authority, path, query, fragment, 6)
      }
      else if (s == 7) {
        val query = this.query.asInstanceOf[Parser[Query]].interpolate(part)
        if (query.isError) query.asError
        else new UriParser(scheme, authority, path, query, fragment, 7)
      }
      else if (s == 8) {
        val fragment = this.fragment.asInstanceOf[Parser[Fragment]].interpolate(part)
        if (fragment.isError) fragment.asError
        else new UriParser(scheme, authority, path, query, fragment, 8)
      }
      else super.interpolate(part)

    override def state: Maybe[Uri] = Bind(bind)

    override def bind: Uri = Uri(scheme.bind, authority.bind, path.bind, query.bind, fragment.bind)

    override def toString: String = (String.Builder~Uri.toString~'.'~"UriParser").state
  }


  private final class SchemeParser(
      builder: StringBuilder with State[String],
      s: Int)
    extends Parser[Scheme] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Scheme] = {
      var c = 0
      var s = this.s
      val builder = if (this.builder ne null) this.builder else String.Builder
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; isAlpha(c) }) {
          input.step()
          builder.append(java.lang.Character.toLowerCase(c))
          s = 2
        }
        else if (!input.isEmpty || input.isDone) return error(input, expected = "scheme", found = c)
      }
      if (s == 2) {
        while (!input.isEmpty && { c = input.head; isSchemeChar(c) }) {
          input.step()
          builder.append(java.lang.Character.toLowerCase(c))
        }
        if (!input.isEmpty || input.isDone) return Iteratee.done(Scheme.Part(builder.state))
      }
      new SchemeParser(builder, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Scheme] =
      if (s == 1) part match {
        case Scheme(scheme) => Iteratee.done(scheme)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[Scheme] = Bind(bind)

    override def bind: Scheme = if (builder ne null) Scheme.Part(builder.state) else Scheme.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"SchemeParser").state
  }


  private final class AuthorityParser(
      userInfo: Iteratee[Int, UserInfo],
      host: Iteratee[Int, Host],
      port: Iteratee[Int, Port],
      s: Int)
    extends Parser[Authority] {

    def this() = this(UserInfoParser, HostParser, PortParser, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Authority] = {
      var c = 0
      var s = this.s
      var port = this.port
      var host = this.host
      var userInfo = this.userInfo
      if (s == 1) {
        if (!input.isEmpty) {
          val look = input.dup
          while (!look.isEmpty && { c = look.head; c != '@' && c != '/' }) look.step()
          if (!look.isEmpty && c == '@') s = 2
          else s = 3
        }
        else if (input.isDone) s = 3
      }
      if (s == 2) {
        userInfo = userInfo.feed(input)
        if (userInfo.isError) return userInfo.asError
        else if (!input.isEmpty && { c = input.head; c == '@' }) {
          input.step()
          s = 3
        }
        else if (!input.isEmpty) return error(input, expected = '@', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 3) {
        host = host.feed(input)
        if (host.isError) return host.asError
        else if (!input.isEmpty && input.head == ':') {
          input.step()
          s = 4
        }
        else if (!input.isEmpty || input.isDone)
          return Iteratee.done(Authority(host.bind, port.bind, userInfo.bind))
      }
      if (s == 4) {
        port = port.feed(input)
        if (port.isError) return port.asError
        else if (!input.isEmpty || input.isDone)
          return Iteratee.done(Authority(host.bind, port.bind, userInfo.bind))
      }
      new AuthorityParser(userInfo, host, port, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Authority] =
      if (s == 1) part match {
        case Authority(authority) => Iteratee.done(authority)
        case UserInfo(userInfo) =>
          val userInfo = this.userInfo.asInstanceOf[Parser[UserInfo]].interpolate(part)
          new AuthorityParser(userInfo, host, port, 2)
        case Host(host) =>
          val host = this.host.asInstanceOf[Parser[Host]].interpolate(part)
          new AuthorityParser(userInfo, host, port, 3)
        case _ => super.interpolate(part)
      }
      else if (s == 2) {
        val userInfo = this.userInfo.asInstanceOf[Parser[UserInfo]].interpolate(part)
        new AuthorityParser(userInfo, host, port, 2)
      }
      else if (s == 3) {
        val host = this.host.asInstanceOf[Parser[Host]].interpolate(part)
        new AuthorityParser(userInfo, host, port, 3)
      }
      else if (s == 4) {
        val port = this.port.asInstanceOf[Parser[Port]].interpolate(part)
        new AuthorityParser(userInfo, host, port, 4)
      }
      else super.interpolate(part)

    override def state: Maybe[Authority] = Bind(bind)

    override def bind: Authority = Authority(host.bind, port.bind, userInfo.bind)

    override def toString: String = (String.Builder~Uri.toString~'.'~"Authority").state
  }


  private final class UserInfoParser(
      username: Builder[Int] with State[String],
      password: Builder[Int] with State[String],
      c1: Int,
      s: Int)
    extends Parser[UserInfo] {

    def this() = this(null, null, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, UserInfo] = {
      var c = 0
      var s = this.s
      var c1 = this.c1
      var password = this.password
      var username = this.username
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          if ((username eq null) && !input.isEmpty) username = UTF8.Decoder(String.Builder)
          while (!input.isEmpty && { c = input.head; isUserChar(c) }) {
            input.step()
            username.append(c)
          }
          if (!input.isEmpty && c == ':') {
            input.step()
            s = 4
          }
          else if (!input.isEmpty && c == '%') {
            input.step()
            s = 2
          }
          else if (!input.isEmpty || input.isDone) return Iteratee.done(UserInfo.Part(username.state))
        }
        if (s == 2) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 3
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            username.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            s = 1
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 4) {
          if (password eq null) password = UTF8.Decoder(String.Builder)
          while (!input.isEmpty && { c = input.head; isUserInfoChar(c) }) {
            input.step()
            password.append(c)
          }
          if (!input.isEmpty && c == '%') {
            input.step()
            s = 5
          }
          else if (!input.isEmpty || input.isDone)
            return Iteratee.done(UserInfo(username.state, password.state))
        }
        if (s == 5) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 6
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 6) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            password.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            s = 4
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new UserInfoParser(username, password, c1, s)
    }

    override def interpolate(part: Part): Iteratee[Int, UserInfo] =
      if (s == 1 && (username eq null)) part match {
        case UserInfo(userInfo) => Iteratee.done(userInfo)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[UserInfo] = Bind(bind)

    override def bind: UserInfo =
      if (password ne null) UserInfo(username.state, password.state)
      else if (username ne null) UserInfo.Part(username.state)
      else UserInfo.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"UserInfoParser").state
  }


  private final class HostParser extends Parser[Host] {
    override def feed(input: Iterator[Int]): Iteratee[Int, Host] = {
      if (!input.isEmpty) {
        val c = input.head
        if (c == '[') HostLiteralParser.feed(input)
        else HostAddressParser.feed(input)
      }
      else this
    }

    override def interpolate(part: Part): Iteratee[Int, Host] = part match {
      case Host(host) => Iteratee.done(host)
      case _ => super.interpolate(part)
    }

    override def state: Maybe[Host] = Bind(bind)

    override def bind: Host = Host.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"HostParser").state
  }


  private final class HostAddressParser(
      builder: Builder[Int] with State[String],
      c1: Int,
      x: Int,
      s: Int)
    extends Parser[Host] {

    def this() = this(null, 0, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Host] = {
      var c = 0
      var s = this.s
      var x = this.x
      var c1 = this.c1
      var builder = this.builder
      while (s <= 4 && (!input.isEmpty || input.isDone)) {
        if (builder eq null) builder = UTF8.Decoder(String.Builder)
        while (!input.isEmpty && { c = input.head; isDigit(c) }) {
          input.step()
          builder.append(c)
          x = 10 * x + decodeDigit(c)
        }
        if (!input.isEmpty) {
          if (c == '.' && s < 4 && x <= 255) {
            input.step()
            builder.append(c)
            x = 0
            s += 1
          }
          else if (!isHostChar(c) && c != '%' && s == 4 && x <= 255)
            return Iteratee.done(Host.IPv4(builder.state))
          else {
            x = 0
            s = 5
          }
        }
        else if (input.isDone) {
          if (s == 4 && x <= 255) return Iteratee.done(Host.IPv4(builder.state))
          else return Iteratee.done(Host.Name(builder.state))
        }
      }
      while (!input.isEmpty || input.isDone) {
        if (s == 5) {
          while (!input.isEmpty && { c = input.head; isHostChar(c) }) {
            input.step()
            builder.append(java.lang.Character.toLowerCase(c))
          }
          if (!input.isEmpty && c == '%') {
            input.step()
            s = 6
          }
          else if (!input.isEmpty || input.isDone) return Iteratee.done(Host.Name(builder.state))
        }
        if (s == 6) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 7
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 7) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            builder.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            s = 5
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new HostAddressParser(builder, c1, x, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Host] =
      if (builder eq null) part match {
        case Host(host) => Iteratee.done(host)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[Host] = Bind(bind)

    override def bind: Host =
      if (builder ne null) {
        if (s == 4 && x <= 255) Host.IPv4(builder.state)
        else Host.Name(builder.state)
      }
      else Host.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"HostAddressParser").state
  }


  private final class HostLiteralParser(
      builder: StringBuilder with State[String],
      s: Int)
    extends Parser[Host] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Host] = {
      var c = 0
      var s = this.s
      var builder = this.builder
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; c == '[' }) {
          input.step()
          s = 2
        }
        else if (!input.isEmpty) return error(input, expected = '[', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 2) {
        if (builder eq null) builder = String.Builder
        while (!input.isEmpty && { c = input.head; isHostChar(c) || c == ':' }) {
          input.step()
          builder.append(java.lang.Character.toLowerCase(c))
        }
        if (!input.isEmpty && c == ']') {
          input.step()
          return Iteratee.done(Host.IPv6(builder.state))
        }
        else if (!input.isEmpty) return error(input, expected = "", found = c)
        else if (input.isDone) return unexpectedEOF
      }
      new HostLiteralParser(builder, s)
    }

    override def state: Maybe[Host] = Bind(bind)

    override def bind: Host = if (builder ne null) Host.IPv6(builder.state) else Host.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"HostLiteralParser").state
  }


  final class PortParser(port: Int) extends Parser[Port] {
    def this() = this(-1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Port] = {
      var c = 0
      var port = if (this.port > 0 || input.isEmpty) this.port else 0
      while (!input.isEmpty && { c = input.head; isDigit(c) }) {
        input.step()
        port = 10 * port + decodeDigit(c)
      }
      if (!input.isEmpty || input.isDone) return Iteratee.done(Port(port))
      new PortParser(port)
    }

    override def interpolate(part: Part): Iteratee[Int, Port] =
      if (port < 0) part match {
        case Port(port) => Iteratee.done(port)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[Port] = Bind(bind)

    override def bind: Port = Port(if (port > 0) port else 0)

    override def toString: String = (String.Builder~Uri.toString~'.'~"PortParser").state
  }


  final class PathParser(
      builder: Builder[Int] with State[String],
      path: PathBuilder,
      c1: Int,
      s: Int)
    extends Parser[Path] {

    def this(c: Int) = {
      this(null, Path.Builder, 0, 1)
      path.append(Path.SegmentSlash)
    }

    def this() = this(null, null, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Path] = {
      var c = 0
      var s = this.s
      var c1 = this.c1
      var path = this.path
      var builder = this.builder
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          while (!input.isEmpty && { c = input.head; isPathChar(c) }) {
            if (builder eq null) builder = UTF8.Decoder(String.Builder)
            input.step()
            builder.append(c)
          }
          if (!input.isEmpty && c == '/') {
            input.step()
            if (path eq null) path = Path.Builder
            if (builder ne null) {
              path.append(Path.Segment(builder.state))
              builder = null
            }
            path.append(Path.SegmentSlash)
          }
          else if (!input.isEmpty && c == '%') {
            input.step()
            s = 2
          }
          else if (!input.isEmpty || input.isDone) {
            if (builder ne null) {
              if (path eq null) path = Path.Builder
              path.append(Path.Segment(builder.state))
            }
            return Iteratee.done(if (path ne null) path.state else Path.Empty)
          }
        }
        if (s == 2) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 3
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            if (builder eq null) builder = UTF8.Decoder(String.Builder)
            input.step()
            builder.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            s = 1
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new PathParser(builder, path, c1, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Path] =
      if (s == 1) part match {
        case Path(subpath) =>
          val path = if (this.path ne null) this.path else Path.Builder
          if (builder ne null) path.append(Path.Segment(builder.state))
          path.appendPath(subpath)
          new PathParser(null, path, 0, 1)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[Path] = Bind(bind)

    override def bind: Path = if (path ne null) path.state else Path.Empty

    override def toString: String = (String.Builder~Uri.toString~'.'~"PathParser").state
  }


  private final class QueryParser(
      key: Builder[Int] with State[String],
      value: Builder[Int] with State[String],
      query: Builder[QueryParam] with State[Query],
      c1: Int,
      s: Int)
    extends Parser[Query] {

    def this() = this(null, null, null, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Query] = {
      var c = 0
      var s = this.s
      var c1 = this.c1
      var query = this.query
      var value = this.value
      var key = this.key
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          if (key eq null) key = UTF8.Decoder(String.Builder)
          while (!input.isEmpty && { c = input.head; isParamChar(c) }) {
            input.step()
            key.append(c)
          }
          if (!input.isEmpty && c == '=') {
            input.step()
            s = 4
          }
          else if (!input.isEmpty && c == '&') {
            input.step()
            if (query eq null) query = Query.Builder
            if (query.isInstanceOf[QueryBuilder])
              query.asInstanceOf[QueryBuilder].append("", key.state)
            else query.append(Query.Param("", value.state))
            key = null
            s = 1
          }
          else if (!input.isEmpty && c == '%') {
            input.step()
            s = 2
          }
          else if (!input.isEmpty || input.isDone) return Iteratee.done {
            if (query eq null) Query.Part(key.state)
            else {
              if (query.isInstanceOf[QueryBuilder])
                query.asInstanceOf[QueryBuilder].append("", key.state)
              else query.append(Query.Param("", key.state))
              query.state
            }
          }
        }
        if (s == 2) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 3
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            key.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            s = 1
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 4) {
          if (value eq null) value = UTF8.Decoder(String.Builder)
          while (!input.isEmpty && { c = input.head; isParamChar(c) || c == '=' }) {
            input.step()
            value.append(c)
          }
          if (!input.isEmpty && c == '&') {
            input.step()
            if (query eq null) query = Query.Builder
            if (query.isInstanceOf[QueryBuilder])
              query.asInstanceOf[QueryBuilder].append(key.state, value.state)
            else query.append(Query.Param(key.state, value.state))
            key = null
            value = null
            s = 1
          }
          else if (!input.isEmpty && c == '%') {
            input.step()
            s = 5
          }
          else if (!input.isEmpty || input.isDone) {
            if (query eq null) query = Query.Builder
            if (query.isInstanceOf[QueryBuilder])
              query.asInstanceOf[QueryBuilder].append(key.state, value.state)
            else query.append(Query.Param(key.state, value.state))
            return Iteratee.done(query.state)
          }
        }
        if (s == 5) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 6
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 6) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            value.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            s = 4
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new QueryParser(key, value, query, c1, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Query] =
      if ((key eq null) && (value eq null) && (query eq null)) part match {
        case Query(query) => Iteratee.done(query)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[Query] = Bind(bind)

    override def bind: Query = if (query ne null) query.state else Query.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"QueryParser").state
  }


  private final class FragmentParser(
      builder: Builder[Int] with State[String],
      c1: Int,
      l: Int,
      s: Int)
    extends Parser[Fragment] {

    def this() = this(null, 0, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Fragment] = {
      var c = 0
      var s = this.s
      var l = this.l
      var c1 = this.c1
      val builder = if (this.builder ne null) this.builder else UTF8.Decoder(String.Builder)
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          while (!input.isEmpty && { c = input.head; isFragmentChar(c) }) {
            input.step()
            builder.append(c)
            l += 1
          }
          if (!input.isEmpty && c == '%') {
            input.step()
            s = 2
          }
          else if (!input.isEmpty || input.isDone) return Iteratee.done(Fragment.Part(builder.state))
        }
        if (s == 2) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            c1 = c
            s = 3
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          if (!input.isEmpty && { c = input.head; isHexChar(c) }) {
            input.step()
            builder.append((decodeHex(c1) << 4) + decodeHex(c))
            c1 = 0
            l += 1
            s = 1
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new FragmentParser(builder, c1, l, s)
    }

    override def interpolate(part: Part): Iteratee[Int, Fragment] =
      if (l == 0) part match {
        case Fragment(fragment) => Iteratee.done(fragment)
        case _ => super.interpolate(part)
      }
      else super.interpolate(part)

    override def state: Maybe[Fragment] = Bind(bind)

    override def bind: Fragment = if (builder ne null) Fragment.Part(builder.state) else Fragment.Undefined

    override def toString: String = (String.Builder~Uri.toString~'.'~"FragmentParser").state
  }


  private[net] def unexpectedEOF: Iteratee[Any, Nothing] =
    Iteratee.error(new UriException("unexpected end of input", Iterator.done))

  private[net] def error(input: Iterator[Int], expected: Int, found: Int): Iteratee[Any, Nothing] = {
    val message =
      if (expected == 0 && found == 0) "unexpected input"
      else if (expected == 0) (String.Builder~"unexpected "~'\''~found~'\'').state
      else if (found == 0) (String.Builder~"expected "~'\''~expected~'\'').state
      else (String.Builder~"expected "~'\''~expected~'\''~", but found "~'\''~found~'\'').state
    Iteratee.error(new UriException(message, input))
  }

  private[net] def error(input: Iterator[Int], expected: String, found: Int): Iteratee[Any, Nothing] = {
    val message =
      if (expected.length == 0 && found == 0) "unexpected input"
      else if (expected.length == 0) (String.Builder~"unexpected "~'\''~found~'\'').state
      else if (found == 0) (String.Builder~"expected "~expected).state
      else (String.Builder~"expected "~expected~", but found "~'\''~found~'\'').state
    Iteratee.error(new UriException(message, input))
  }
}
