//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.text._
import basis.util._

sealed abstract class UserInfo private[net] extends UriPart {
  def isDefined: Boolean

  def username: String

  def password: String

  def part: String

  def writeUriString(builder: StringBuilder): Unit

  def toUriString: String = {
    val builder = String.Builder
    writeUriString(builder)
    builder.state
  }

  override def toString: String = {
    val s = String.Builder~"UserInfo"~'('~'"'
    writeUriString(s)
    (s~'"'~')').state
  }
}

object UserInfo extends Uri.UserInfoFactory {
  override val Undefined: UserInfo = new Undefined()

  override def Part(userInfo: String): UserInfo = new Part(userInfo)

  override def apply(username: String, password: String): UserInfo =
    new Login(username, password)

  implicit def apply(userInfo: String): UserInfo = {
    val input = new UString(userInfo).iterator
    var result = Uri.UserInfoParser.run(input)
    if (!input.isEmpty)
      result = Uri.error(input, expected = "valid user info character", found = input.head)
    if (!result.isError) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new UriException(error.toString)
    }
  }

  override def unapply(part: UriPart): Maybe[UserInfo] = part match {
    case userInfo: UserInfo => Bind(userInfo)
    case _ => Trap
  }

  override def toString: String = "UserInfo"


  private[net] final class Login(
      override val username: String,
      override val password: String)
    extends UserInfo {

    override def isDefined: Boolean = true

    override def part: String = {
      val builder = String.Builder
      builder.append(username)
      builder.append(':')
      builder.append(password)
      builder.state
    }

    override def writeUriString(builder: StringBuilder): Unit = {
      Uri.writeUser(username)(builder)
      builder.append(':')
      Uri.writeUserInfo(password)(builder)
    }

    override def equals(other: Any): Boolean =
      eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Login] && {
        val that = other.asInstanceOf[Login]
        username.equals(that.username) && password.equals(that.password)
      }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Login], username.hashCode), password.hashCode))
    }
  }

  private[net] final class Part(override val part: String) extends UserInfo {
    override def isDefined: Boolean = true

    override def username: String = part

    override def password: String = ""

    override def writeUriString(builder: StringBuilder): Unit =
      Uri.writeUserInfo(part)(builder)

    override def equals(other: Any): Boolean =
      eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Part] &&
      part.equals(other.asInstanceOf[Part].part)

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Part], part.hashCode))
    }
  }

  private[net] final class Undefined extends UserInfo {
    override def isDefined: Boolean = false

    override def username: String = ""

    override def password: String = ""

    override def part: String = ""

    override def writeUriString(builder: StringBuilder): Unit = ()

    override def toUriString: String = ""

    override def toString: String = (String.Builder~"UserInfo"~'.'~"Undefined").state
  }
}
