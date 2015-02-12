//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.collections._
import basis.text._

trait UriParser { UriParser =>
  type Uri
  type Scheme
  type Authority
  type UserInfo
  type Host
  type Port
  type Path
  type Query
  type Fragment

  def Scheme(scheme: String): Scheme

  def Authority(host: Host, port: Port, userInfo: UserInfo): Authority

  def UserInfo(userInfo: String): UserInfo

  def NameHost(address: String): Host

  def IPv4Host(address: String): Host

  def IPv6Host(address: String): Host

  def EmptyHost: Host

  def Port(port: Int): Port

  def EmptyPath: Path

  def PathBuilder: Builder[String] with State[Path]

  lazy val UriParser: Iteratee[Int, Uri] = Predef.???

  lazy val SchemeParser: Iteratee[Int, Scheme] = new SchemeParser()

  lazy val AuthorityParser: Iteratee[Int, Authority] = new AuthorityParser()

  lazy val UserInfoParser: Iteratee[Int, UserInfo] = new UserInfoParser()

  lazy val HostParser: Iteratee[Int, Host] = new HostParser()

  lazy val HostAddressParser: Iteratee[Int, Host] = new HostAddressParser()

  lazy val HostLiteralParser: Iteratee[Int, Host] = new HostLiteralParser()

  lazy val PortParser: Iteratee[Int, Port] = new PortParser()

  lazy val PathParser: Iteratee[Int, Path] = new PathParser()

  lazy val QueryParser: Iteratee[Int, Query] = Predef.???

  lazy val FragmentParser: Iteratee[Int, Fragment] = Predef.???


  protected def isUnreservedChar(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '-' || c == '.' ||
    c == '_' || c == '~'

  protected def isSubDelimChar(c: Int): Boolean =
    c == '!' || c == '$' ||
    c == '&' || c == '\'' ||
    c == '(' || c == ')' ||
    c == '*' || c == '+' ||
    c == ',' || c == ';' ||
    c == '='

  protected def isSchemeChar(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '+' || c == '-' ||
    c == '.'

  protected def isUserInfoChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c) ||
    c == ':'

  protected def isHostChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c)

  protected def isPathChar(c: Int): Boolean =
    isUnreservedChar(c) ||
    isSubDelimChar(c) ||
    c == ':' || c == '@'

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


  private final class SchemeParser(
      builder: StringBuilder with State[String])
    extends Iteratee[Int, Scheme] {

    def this() = this(null)

    override def feed(input: Iterator[Int]): Iteratee[Int, Scheme] = {
      var c = 0
      val builder = if (this.builder ne null) this.builder else String.Builder
      while (!input.isEmpty && { c = input.head; isSchemeChar(c) }) {
        input.step()
        builder.append(c)
      }
      if (!input.isEmpty || input.isDone) return Iteratee.done(Scheme(builder.state))
      new SchemeParser(builder)
    }

    override def state: Maybe[Scheme] = Bind(bind)

    override def bind: Scheme = Scheme(if (builder ne null) builder.state else "")

    override def toString: String = (String.Builder~UriParser.toString~'.'~"SchemeParser").state
  }


  private final class AuthorityParser(
      backtrack: Iterator[Int],
      userInfo: Iteratee[Int, UserInfo],
      host: Iteratee[Int, Host],
      port: Iteratee[Int, Port],
      s: Int)
    extends Iteratee[Int, Authority] {

    def this() = this(null.asInstanceOf[Iterator[Int]], UserInfoParser, HostParser, PortParser, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Authority] = {
      var c = 0
      var s = this.s
      var port = this.port
      var host = this.host
      var userInfo = this.userInfo
      val backtrack = if (this.backtrack.asInstanceOf[AnyRef] ne null) this.backtrack else input.dup
      if (s == 1) {
        while (!input.isEmpty && { c = input.head; c != '@' && c != '/' }) input.step()
        if (!input.isEmpty && c == '@') {
          while (!input.isEmpty && { c = input.head; c != '/' }) input.step()
          s = 2
        }
        else s = 4
      }
      if (s == 2) {
        userInfo = userInfo.feed(backtrack)
        if (!userInfo.isError) s = 3
        else if (userInfo.isError) return userInfo.asError
      }
      if (s == 3) {
        if (!backtrack.isEmpty && { c = backtrack.head; c == '@' }) {
          backtrack.step()
          s = 4
        }
        else if (!backtrack.isEmpty) return error(backtrack, expected = '@', found = c)
        else if (backtrack.isDone) return unexpectedEOF
      }
      if (s == 4) {
        host = host.feed(backtrack)
        if (!host.isError) s = 5
        else if (host.isError) return host.asError
      }
      if (s == 5) {
        if (!backtrack.isEmpty && backtrack.head == ':') {
          backtrack.step()
          s = 6
        }
        else return Iteratee.done(Authority(host.bind, port.bind, userInfo.bind))
      }
      if (s == 6) {
        port = port.feed(backtrack)
        if (!port.isError) return Iteratee.done(Authority(host.bind, port.bind, userInfo.bind))
        else if (port.isError) return port.asError
      }
      new AuthorityParser(backtrack, userInfo, host, port, s)
    }

    override def toString: String = (String.Builder~UriParser.toString~'.'~"Authority").state
  }


  private final class UserInfoParser(
      builder: Builder[Int] with State[String],
      c1: Int,
      s: Int)
    extends Iteratee[Int, UserInfo] {

    def this() = this(null, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, UserInfo] = {
      var c = 0
      var s = this.s
      var c1 = this.c1
      val builder = if (this.builder ne null) this.builder else UTF8.Decoder(String.Builder)
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          while (!input.isEmpty && { c = input.head; isUserInfoChar(c) }) {
            input.step()
            builder.append(c)
          }
          if (!input.isEmpty && c == '%') {
            input.step()
            s = 2
          }
          else return Iteratee.done(UserInfo(builder.state))
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
            s = 1
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new UserInfoParser(builder, c1, s)
    }

    override def state: Maybe[UserInfo] = Bind(bind)

    override def bind: UserInfo = UserInfo(if (builder ne null) builder.state else "")

    override def toString: String = (String.Builder~UriParser.toString~'.'~"UserInfoParser").state
  }


  private final class HostParser extends Iteratee[Int, Host] {
    override def feed(input: Iterator[Int]): Iteratee[Int, Host] = {
      if (!input.isEmpty) {
        val c = input.head
        if (c == '[') HostLiteralParser.feed(input)
        else HostAddressParser.feed(input)
      }
      else this
    }

    override def state: Maybe[Host] = Bind(bind)

    override def bind: Host = EmptyHost

    override def toString: String = (String.Builder~UriParser.toString~'.'~"HostParser").state
  }


  private final class HostAddressParser(
      builder: Builder[Int] with State[String],
      c1: Int,
      x: Int,
      s: Int)
    extends Iteratee[Int, Host] {

    def this() = this(null, 0, 0, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Host] = {
      var c = 0
      var s = this.s
      var x = this.x
      var c1 = this.c1
      val builder = if (this.builder ne null) this.builder else UTF8.Decoder(String.Builder)
      while (s <= 4 && (!input.isEmpty || input.isDone)) {
        while (!input.isEmpty && { c = input.head; isDigit(c) }) {
          input.step()
          builder.append(c)
          x = 10 * x + decodeDigit(c)
        }
        if (s < 4 && x <= 255 && !input.isEmpty && c == '.') {
          input.step()
          builder.append(c)
          x = 0
          s += 1
        }
        else if (s == 4 && x <= 255 &&
            (!input.isEmpty && !isHostChar(c) && c != '%' || input.isEmpty || input.isDone))
          return Iteratee.done(IPv4Host(builder.state))
        else {
          x = 0
          s = 5
        }
      }
      while (!input.isEmpty || input.isDone) {
        if (s == 5) {
          while (!input.isEmpty && { c = input.head; isHostChar(c) }) {
            input.step()
            builder.append(c)
          }
          if (!input.isEmpty && c == '%') {
            input.step()
            s = 6
          }
          else return Iteratee.done(NameHost(builder.state))
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

    override def state: Maybe[Host] = Bind(bind)

    override def bind: Host = NameHost(if (builder ne null) builder.state else "")

    override def toString: String = (String.Builder~UriParser.toString~'.'~"HostAddressParser").state
  }


  private final class HostLiteralParser(
      builder: StringBuilder with State[String],
      s: Int)
    extends Iteratee[Int, Host] {

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
          builder.append(c)
        }
        if (!input.isEmpty && c == ']') {
          input.step()
          return Iteratee.done(IPv6Host(builder.state))
        }
        else if (!input.isEmpty) return error(input, expected = "", found = c)
        else if (input.isDone) return unexpectedEOF
      }
      new HostLiteralParser(builder, s)
    }

    override def state: Maybe[Host] = Bind(bind)

    override def bind: Host = IPv6Host(if (builder ne null) builder.state else "")

    override def toString: String = (String.Builder~UriParser.toString~'.'~"HostLiteralParser").state
  }


  final class PortParser(port: Int) extends Iteratee[Int, Port] {
    def this() = this(0)

    override def feed(input: Iterator[Int]): Iteratee[Int, Port] = {
      var c = 0
      var port = this.port
      while (!input.isEmpty && { c = input.head; isDigit(c) }) {
        input.step()
        port = 10 * port + decodeDigit(c)
      }
      Iteratee.done(Port(port))
    }

    override def state: Maybe[Port] = Bind(bind)

    override def bind: Port = Port(port)

    override def toString: String = (String.Builder~UriParser.toString~'.'~"PortParser").state
  }


  final class PathParser(
      builder: Builder[Int] with State[String],
      path: Builder[String] with State[Path],
      c1: Int,
      s: Int)
    extends Iteratee[Int, Path] {

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
            if (path eq null) path = PathBuilder
            if (builder ne null) {
              path.append(builder.state)
              builder = null
            }
            path.append("/")
          }
          else if (!input.isEmpty && c == '%') {
            input.step()
            s = 2
          }
          else if (builder ne null) {
            if (path eq null) path = PathBuilder
            path.append(builder.state)
            return Iteratee.done(path.state)
          }
          else return Iteratee.done(if (path ne null) path.state else EmptyPath)
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
            s = 1
          }
          else if (!input.isEmpty) return error(input, expected = "hex digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new PathParser(builder, path, c1, s)
    }

    override def state: Maybe[Path] = Bind(bind)

    override def bind: Path = if (path ne null) path.state else EmptyPath

    override def toString: String = (String.Builder~UriParser.toString~'.'~"PathParser").state
  }


  private def unexpectedEOF: Iteratee[Any, Nothing] =
    Iteratee.error(new UriException("unexpected end of input", Iterator.done))

  private def error(input: Iterator[Int], expected: Int, found: Int): Iteratee[Any, Nothing] = {
    val message =
      if (expected == 0 && found == 0) "unexpected input"
      else if (expected == 0) (String.Builder~"unexpected "~'\''~found~'\'').state
      else if (found == 0) (String.Builder~"expected "~'\''~expected~'\'').state
      else (String.Builder~"expected "~'\''~expected~'\''~", but found "~'\''~found~'\'').state
    Iteratee.error(new UriException(message, input))
  }

  private def error(input: Iterator[Int], expected: String, found: Int): Iteratee[Any, Nothing] = {
    val message =
      if (expected.length == 0 && found == 0) "unexpected input"
      else if (expected.length == 0) (String.Builder~"unexpected "~'\''~found~'\'').state
      else if (found == 0) (String.Builder~"expected "~expected).state
      else (String.Builder~"expected "~expected~", but found "~'\''~found~'\'').state
    Iteratee.error(new UriException(message, input))
  }
}
