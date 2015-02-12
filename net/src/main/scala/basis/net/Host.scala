//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.text._
import basis.util._

sealed abstract class Host private[net] extends UriPart {
  def isDefined: Boolean

  def isName: Boolean = false
  def isIPv4: Boolean = false
  def isIPv6: Boolean = false

  def address: String

  def writeUriString(builder: StringBuilder): Unit

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }
}

object Host extends Uri.HostFactory {
  override val Undefined: Host = new Undefined()

  override def Name(address: String): Host = new Name(address)

  override def IPv4(address: String): Host = new IPv4(address)

  override def IPv6(address: String): Host = new IPv6(address)

  implicit def apply(host: String): Host = {
    val input = new UString(host).iterator
    var result = Uri.HostParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid host character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Host] = part match {
    case host: Host => Bind(host)
    case _ => Trap
  }

  override def toString: String = "Host"


  private[net] final class Name(override val address: String) extends Host {
    override def isDefined: Boolean = true

    override def isName: Boolean = true

    override def writeUriString(builder: StringBuilder): Unit =
      Uri.writeHost(address)(builder)

    override def equals(other: Any): Boolean =
      eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Name] &&
      address.equals(other.asInstanceOf[Name].address)

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Name], address.hashCode))
    }

    override def toString: String = (String.Builder~"Host"~'.'~"Name"~'('~>address~')').state
  }

  private[net] final class IPv4(override val address: String) extends Host {
    override def isDefined: Boolean = true

    override def isIPv4: Boolean = true

    override def writeUriString(builder: StringBuilder): Unit =
      Uri.writeHost(address)(builder)

    override def equals(other: Any): Boolean =
      eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[IPv4] &&
      address.equals(other.asInstanceOf[IPv4].address)

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[IPv4], address.hashCode))
    }

    override def toString: String = (String.Builder~"Host"~'.'~"IPv4"~'('~>address~')').state
  }

  private[net] final class IPv6(override val address: String) extends Host {
    override def isDefined: Boolean = true

    override def isIPv6: Boolean = true

    override def writeUriString(builder: StringBuilder): Unit = {
      builder.append('[')
      Uri.writeHost(address)(builder)
      builder.append(']')
    }

    override def equals(other: Any): Boolean =
      eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[IPv6] &&
      address.equals(other.asInstanceOf[IPv6].address)

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[IPv6], address.hashCode))
    }

    override def toString: String = (String.Builder~"Host"~'.'~"IPv6"~'('~>address~')').state
  }

  private[net] final class Undefined extends Host {
    override def isDefined: Boolean = false

    override def address: String = ""

    override def writeUriString(builder: StringBuilder): Unit = ()

    override def toString: String = (String.Builder~"Host"~'.'~"Undefined").state
  }
}
