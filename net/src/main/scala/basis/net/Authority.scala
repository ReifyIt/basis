//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis.text._
import basis.util._

final class Authority private[net] (val host: Host, val port: Int, val userInfo: String) {
  def isEmpty: Boolean = host.isEmpty && port == 0 && userInfo.length == 0

  def writeUriString(builder: StringBuilder): Unit = {
    if (userInfo.length > 0) {
      var i = 0
      val n = userInfo.length
      while (i < n) {
        val c = userInfo.codePointAt(i)
        if (Uri.isUnreservedChar(c) || c == ':') builder.append(c)
        else Uri.writeEncoded(c)
        i = userInfo.offsetByCodePoints(i, 1)
      }
      builder.append('@')
    }
    host.writeUriString(builder)
    if (port != 0) {
      builder.append(':')
      builder.append(java.lang.Integer.toString(port))
    }
  }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def equals(other: Any): Boolean = other match {
    case that: Authority =>
      host.equals(that.host) && port == that.port && userInfo.equals(that.userInfo)
    case _ => false
  }

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(mix(mix(seed[Authority], host.hashCode), port), userInfo.hashCode))
  }

  override def toString: String = {
    val s = String.Builder~"Authority"~'('
    s~"host"~" = "~>host
    if (port != 0) s~", "~"port"~" = "~>port
    if (userInfo.length > 0) s~", "~"userInfo"~" = "~>userInfo
    (s~')').state
  }
}

object Authority {
  val empty: Authority = new Authority(Host.empty, 0, "")

  def apply(host: Host, port: Int = 0, userInfo: String = ""): Authority =
    if (host.isEmpty && port == 0 && userInfo.length == 0) empty
    else new Authority(host, port, userInfo)

  def apply(authority: String): Authority = {
    val result = Uri.Parser.AuthorityParser.run(new UString(authority).iterator)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def toString: String = "Authority"
}
