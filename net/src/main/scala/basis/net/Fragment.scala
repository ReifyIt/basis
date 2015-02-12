//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.text._
import basis.util._

final class Fragment private[net] (private val fragment: String) extends UriPart {
  def isDefined: Boolean = fragment ne null

  def part: String = if (fragment ne null) fragment else ""

  def writeUriString(builder: StringBuilder): Unit =
    if (fragment ne null) Uri.writeFragment(fragment)(builder)

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Fragment] &&
    fragment == other.asInstanceOf[Fragment].fragment

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(seed[Fragment], fragment.##))
  }

  override def toString: String = {
    val s = String.Builder~"Fragment"
    if (fragment eq null) s~'.'~"Undefined"
    else {
      s~'('~'"'
      writeUriString(s)
      s~'"'~')'
    }
    s.state
  }
}

object Fragment extends Uri.FragmentFactory {
  override val Undefined: Fragment = new Fragment(null)

  override def Part(fragment: String): Fragment = new Fragment(fragment)

  implicit def apply(fragment: String): Fragment = {
    val input = new UString(fragment).iterator
    var result = Uri.FragmentParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid fragment character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[Fragment] = part match {
    case fragment: Fragment => Bind(fragment)
    case _ => Trap
  }

  override def toString: String = "Fragment"
}
