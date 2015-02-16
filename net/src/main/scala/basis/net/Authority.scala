//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.text._
import basis.util._

final class Authority private[net] (val host: Host, val port: Port, val userInfo: UserInfo) extends UriPart {
  def isDefined: Boolean = host.isDefined || port.isDefined || userInfo.isDefined

  def writeUriString(builder: StringBuilder): Unit = {
    if (userInfo.isDefined) {
      userInfo.writeUriString(builder)
      builder.append('@')
    }
    host.writeUriString(builder)
    if (port.isDefined) {
      builder.append(':')
      port.writeUriString(builder)
    }
  }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  def copy(host: Host = this.host, port: Port = this.port, userInfo: UserInfo = this.userInfo): Authority =
    new Authority(host, port, userInfo)

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Authority] && {
      val that = other.asInstanceOf[Authority]
      host.equals(that.host) && port.equals(that.port) && userInfo.equals(that.userInfo)
    }

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(mix(mix(seed[Authority], host.hashCode), port.hashCode), userInfo.hashCode))
  }

  override def toString: String = {
    val s = String.Builder~"Authority"
    if (!isDefined) s~'.'~"Undefined"
    else {
      s~'('~'"'
      writeUriString(s)
      s~'"'~')'
    }
    s.state
  }
}

object Authority extends Uri.AuthorityFactory {
  override val Undefined: Authority = new Authority(Host.Undefined, Port.Undefined, UserInfo.Undefined)

  override def apply(host: Host, port: Port = Port.Undefined, userInfo: UserInfo = UserInfo.Undefined): Authority =
    if (!host.isDefined && !port.isDefined && !userInfo.isDefined) Undefined
    else new Authority(host, port, userInfo)

  implicit def apply(authority: String): Authority = {
    val input = new UString(authority).iterator
    var result = Uri.AuthorityParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid authority character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Authority] = part match {
    case authority: Authority => Bind(authority)
    case _ => Trap
  }

  override def toString: String = "Authority"
}
