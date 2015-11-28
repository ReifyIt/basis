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

final class Port private[net] (val number: Int) extends UriPart {
  def isDefined: Boolean = number != 0

  def writeUriString(builder: Builder[Int]): Unit = {
    Uri.writePort(number)(builder)
  }

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def equals(other: Any): Boolean = {
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Port] &&
    number == other.asInstanceOf[Port].number
  }

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(seed[Port], number))
  }

  override def toString: String = {
    val s = String.Builder~"Port"
    if (number == 0) s~'.'~"Undefined"
    else s~'('~>number~')'
    s.state
  }
}

object Port extends Uri.PortFactory {
  override val Undefined: Port = new Port(0)

  implicit override def apply(number: Int): Port = {
    if (number == 0) Undefined
    else new Port(number)
  }

  implicit def apply(port: String): Port = {
    val input = new UString(port).iterator
    var result = Uri.PortParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid port character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Port] = part match {
    case port: Port => Bind(port)
    case _ => Trap
  }

  override def toString: String = "Port"
}
