//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.collections._

trait UriFactory {
  type Uri
  type Part
  type Scheme    <: Part
  type Authority <: Part
  type UserInfo  <: Part
  type Host      <: Part
  type Port      <: Part
  type Path      <: Part
  type Query     <: Part
  type Fragment  <: Part
  type PathSegment
  type QueryParam

  val Scheme: SchemeFactory
  val Authority: AuthorityFactory
  val UserInfo: UserInfoFactory
  val Host: HostFactory
  val Port: PortFactory
  val Path: PathFactory
  val Query: QueryFactory
  val Fragment: FragmentFactory

  def apply(
      scheme: Scheme = Scheme.Undefined,
      authority: Authority = Authority.Undefined,
      path: Path = Path.Empty,
      query: Query = Query.Undefined,
      fragment: Fragment = Fragment.Undefined)
    : Uri


  trait SchemeFactory {
    def Undefined: Scheme

    def Part(scheme: String): Scheme

    def unapply(part: Part): Maybe[Scheme]
  }


  trait AuthorityFactory {
    def Undefined: Authority

    def apply(
        host: Host,
        port: Port = Port.Undefined,
        userInfo: UserInfo = UserInfo.Undefined)
      : Authority

    def unapply(part: Part): Maybe[Authority]
  }


  trait UserInfoFactory {
    def Undefined: UserInfo

    def Part(userInfo: String): UserInfo

    def apply(username: String, password: String): UserInfo

    def unapply(part: Part): Maybe[UserInfo]
  }


  trait HostFactory {
    def Undefined: Host

    def Name(address: String): Host

    def IPv4(address: String): Host

    def IPv6(address: String): Host

    def unapply(part: Part): Maybe[Host]
  }


  trait PortFactory {
    def Undefined: Port

    def apply(number: Int): Port

    def unapply(part: Part): Maybe[Port]
  }


  trait PathFactory {
    def Empty: Path

    def Slash: Path

    def SegmentSlash: PathSegment

    def Segment(segment: String): PathSegment

    def Part(segment: PathSegment): Path

    def unapply(part: Part): Maybe[Path]

    def Builder: PathBuilder
  }

  trait PathBuilder extends Builder[PathSegment] with State[Path] {
    def appendPath(path: Path): Unit

    def ++= (path: Path): this.type = { appendPath(path); this }
  }


  trait QueryFactory {
    def Undefined: Query

    def Part(query: String): Query

    def Param(key: String, value: String): QueryParam

    def unapply(part: Part): Maybe[Query]

    def Builder: Builder[QueryParam] with State[Query]
  }


  trait FragmentFactory {
    def Undefined: Fragment

    def Part(fragment: String): Fragment

    def unapply(part: Part): Maybe[Fragment]
  }
}
