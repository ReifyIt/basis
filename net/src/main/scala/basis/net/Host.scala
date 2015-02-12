//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis.text._
import basis.util._

sealed abstract class Host private[net] {
  def isEmpty: Boolean = address.length == 0

  def isName: Boolean = false
  def isIPv4: Boolean = false
  def isIPv6: Boolean = false

  def address: String

  def writeUriString(builder: StringBuilder): Unit = ()

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }
}

object Host {
  def empty: Host = Empty

  def apply(host: String): Host = {
    val result = Uri.Parser.HostParser.run(new UString(host).iterator)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  def Name(address: String): Host = new Name(address)

  def IPv4(address: String): Host = new IPv4(address)

  def IPv6(address: String): Host = new IPv6(address)

  override def toString: String = "Host"


  private[net] final class Name(override val address: String) extends Host {
    override def isName: Boolean = true

    override def writeUriString(builder: StringBuilder): Unit =
      Uri.writeEncoded(address)(builder)

    override def equals(other: Any): Boolean = other match {
      case that: Name => address.equals(that.address)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Name], address.hashCode))
    }

    override def toString: String = (String.Builder~"Host"~'.'~"Name"~'('~>address~')').state
  }

  private[net] final class IPv4(override val address: String) extends Host {
    override def isIPv4: Boolean = true

    override def writeUriString(builder: StringBuilder): Unit =
      Uri.writeEncoded(address)(builder)

    override def equals(other: Any): Boolean = other match {
      case that: IPv4 => address.equals(that.address)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[IPv4], address.hashCode))
    }

    override def toString: String = (String.Builder~"Host"~'.'~"IPv4"~'('~>address~')').state
  }

  private[net] final class IPv6(override val address: String) extends Host {
    override def isIPv6: Boolean = true

    override def writeUriString(builder: StringBuilder): Unit = {
      builder.append('[')
      builder.append(address)
      builder.append(']')
    }

    override def equals(other: Any): Boolean = other match {
      case that: IPv6 => address.equals(that.address)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[IPv6], address.hashCode))
    }

    override def toString: String = (String.Builder~"Host"~'.'~"IPv6"~'('~>address~')').state
  }

  private[net] object Empty extends Host {
    override def isEmpty: Boolean = true

    override def address: String = ""

    override def toString: String = (String.Builder~"Host"~'.'~"empty").state
  }
}
