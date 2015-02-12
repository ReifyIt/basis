//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.text._
import basis.util._

final class Scheme private[net] (val part: String) extends UriPart {
  def isDefined: Boolean = part.length > 0

  def writeUriString(builder: StringBuilder): Unit =
    if (part.length > 0) Uri.writeScheme(part)(builder)

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Scheme] &&
    part.equals(other.asInstanceOf[Scheme].part)

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(seed[Scheme], part.hashCode))
  }

  override def toString: String = {
    val s = String.Builder~"Scheme"
    if (part.length == 0) s~'.'~"Undefined"
    else {
      s~'('~'"'
      writeUriString(s)
      s~'"'~')'
    }
    s.state
  }
}

object Scheme extends Uri.SchemeFactory {
  override val Undefined: Scheme = new Scheme("")

  override def Part(scheme: String): Scheme =
    if (scheme.length == 0) Undefined
    else new Scheme(scheme)

  implicit def apply(scheme: String): Scheme = {
    val input = new UString(scheme).iterator
    var result = Uri.SchemeParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid scheme character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Scheme] = part match {
    case scheme: Scheme => Bind(scheme)
    case _ => Trap
  }

  override def toString: String = "Scheme"
}
