//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis._
import basis.collections._
import scala.reflect.macros._

private[net] class UriExprFactory[C <: blackbox.Context](val c: C) extends UriParser {
  import c.{ Expr, mirror, Tree, WeakTypeTag }
  import c.universe._

  override type Uri         = Expr[basis.net.Uri]
  override type Part        = Expr[basis.net.UriPart]
  override type Scheme      = Expr[basis.net.Scheme]
  override type Authority   = Expr[basis.net.Authority]
  override type UserInfo    = Expr[basis.net.UserInfo]
  override type Host        = Expr[basis.net.Host]
  override type Port        = Expr[basis.net.Port]
  override type Path        = Expr[basis.net.Path]
  override type Query       = Expr[basis.net.Query]
  override type Fragment    = Expr[basis.net.Fragment]
  override type PathSegment = Expr[String]
  override type QueryParam  = Expr[(String, String)]

  implicit protected def UriTag       = WeakTypeTag[basis.net.Uri](mirror.staticClass("basis.net.Uri").toType)
  implicit protected def PartTag      = WeakTypeTag[basis.net.UriPart](mirror.staticClass("basis.net.UriPart").toType)
  implicit protected def SchemeTag    = WeakTypeTag[basis.net.Scheme](mirror.staticClass("basis.net.Scheme").toType)
  implicit protected def AuthorityTag = WeakTypeTag[basis.net.Authority](mirror.staticClass("basis.net.Authority").toType)
  implicit protected def UserInfoTag  = WeakTypeTag[basis.net.UserInfo](mirror.staticClass("basis.net.UserInfo").toType)
  implicit protected def HostTag      = WeakTypeTag[basis.net.Host](mirror.staticClass("basis.net.Host").toType)
  implicit protected def PortTag      = WeakTypeTag[basis.net.Port](mirror.staticClass("basis.net.Port").toType)
  implicit protected def PathTag      = WeakTypeTag[basis.net.Path](mirror.staticClass("basis.net.Path").toType)
  implicit protected def QueryTag     = WeakTypeTag[basis.net.Query](mirror.staticClass("basis.net.Query").toType)
  implicit protected def FragmentTag  = WeakTypeTag[basis.net.Fragment](mirror.staticClass("basis.net.Fragment").toType)
  implicit protected def StringTag    = WeakTypeTag[String](definitions.StringClass.toType)
  implicit protected def Tuple2Tag[A, B](implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(definitions.TupleClass(2).asType.toTypeConstructor, A.tpe :: B.tpe :: Nil))

  override def apply(scheme: Scheme, authority: Authority, path: Path, query: Query, fragment: Fragment): Uri =
    Expr[basis.net.Uri](q"_root_.basis.net.Uri($scheme, $authority, $path, $query, $fragment)")


  object Scheme extends SchemeFactory {
    override def Undefined: Scheme =
      Expr[basis.net.Scheme](q"_root_.basis.net.Scheme.Undefined")

    override def Part(scheme: String): Scheme =
      Expr[basis.net.Scheme](q"_root_.basis.net.Scheme.Part($scheme)")

    override def unapply(part: Part): Maybe[Scheme] =
      if (part.actualType <:< SchemeTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Scheme]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, scheme :: Nil) =>
          basis.Bind(Expr[basis.net.Scheme](q"_root_.basis.net.Scheme($scheme)"))
        case _ => Trap
      }
      else Trap
  }


  object Authority extends AuthorityFactory {
    override def Undefined: Authority =
      Expr[basis.net.Authority](q"_root_.basis.net.Authority.Undefined")

    override def apply(host: Host, port: Port, userInfo: UserInfo): Authority =
      Expr[basis.net.Authority](q"_root_.basis.net.Authority($host, $port, $userInfo)")

    override def unapply(part: Part): Maybe[Authority] =
      if (part.actualType <:< AuthorityTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Authority]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, authority :: Nil) =>
          basis.Bind(Expr[basis.net.Authority](q"_root_.basis.net.Authority($authority)"))
        case _ => Trap
      }
      else Trap
  }


  object UserInfo extends UserInfoFactory {
    override def Undefined: UserInfo =
      Expr[basis.net.UserInfo](q"_root_.basis.net.UserInfo.Undefined")

    override def Part(userInfo: String): UserInfo =
      Expr[basis.net.UserInfo](q"_root_.basis.net.UserInfo.Part($userInfo)")

    override def apply(username: String, password: String): UserInfo =
      Expr[basis.net.UserInfo](q"_root_.basis.net.UserInfo($username, $password)")

    override def unapply(part: Part): Maybe[UserInfo] =
      if (part.actualType <:< UserInfoTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.UserInfo]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, userInfo :: Nil) =>
          basis.Bind(Expr[basis.net.UserInfo](q"_root_.basis.net.UserInfo($userInfo)"))
        case _ => Trap
      }
      else Trap
  }


  object Host extends HostFactory {
    override def Undefined: Host =
      Expr[basis.net.Host](q"_root_.basis.net.Host.Undefined")

    override def Name(address: String): Host =
      Expr[basis.net.Host](q"_root_.basis.net.Host.Name($address)")

    override def IPv4(address: String): Host =
      Expr[basis.net.Host](q"_root_.basis.net.Host.IPv4($address)")

    override def IPv6(address: String): Host =
      Expr[basis.net.Host](q"_root_.basis.net.Host.IPv6($address)")

    override def unapply(part: Part): Maybe[Host] =
      if (part.actualType <:< HostTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Host]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, host :: Nil) =>
          basis.Bind(Expr[basis.net.Host](q"_root_.basis.net.Host($host)"))
        case _ => Trap
      }
      else Trap
  }


  object Port extends PortFactory {
    override def Undefined: Port =
      Expr[basis.net.Port](q"_root_.basis.net.Port.Undefined")

    override def apply(number: Int): Port =
      Expr[basis.net.Port](q"_root_.basis.net.Port($number)")

    override def unapply(part: Part): Maybe[Port] =
      if (part.actualType <:< PortTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Port]])
      else Trap
  }


  object Path extends PathFactory {
    override def Empty: Path =
      Expr[basis.net.Path](q"_root_.basis.net.Path.Empty")

    override def Slash: Path =
      Expr[basis.net.Path](q"_root_.basis.net.Path.Slash")

    override def SegmentSlash: PathSegment =
      Expr[String](Literal(Constant("/")))

    override def Segment(segment: String): PathSegment =
      Expr[String](Literal(Constant(segment)))

    override def unapply(part: Part): Maybe[Path] =
      if (part.actualType <:< PathTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Path]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, path :: Nil) =>
          basis.Bind(Expr[basis.net.Path](q"_root_.basis.net.Path($path)"))
        case _ => Trap
      }
      else Trap

    override def Builder: PathBuilder = new PathExprBuilder()
  }

  private final class PathExprBuilder extends PathBuilder {
    private[this] var builder = null: Tree

    override def append(segment: PathSegment): Unit = {
      if (builder eq null) builder = q"_root_.basis.net.Path.Builder"
      builder = q"$builder += $segment"
    }

    override def appendPath(path: Path): Unit = {
      if (builder eq null) path.tree match {
        case Select(b, TermName("state")) => builder = b
        case _ =>
          builder = q"_root_.basis.net.Path.Builder"
          builder = q"$builder ++= $path"
      }
      else builder = q"$builder ++= $path"
    }

    override def clear(): Unit = builder = null

    override def state: Path = Expr[basis.net.Path] {
      if (builder eq null) q"_root_.basis.net.Path.empty"
      else q"$builder.state"
    }

    //private[this] val self = mutable.ArrayBuffer.empty[PathSegment]
    //override def append(segment: PathSegment): Unit = self.append(segment)
    //override def appendPath(path: Path): Unit = Predef.???
    //override def clear(): Unit = self.clear()
    //override def state: Path = Expr[basis.net.Path] {
    //  if (self.isEmpty) q"_root_.basis.net.Path.Empty"
    //  else {
    //    val builder = self.foldLeft(q"_root_.basis.net.Path.Builder": Tree)((b, e) => q"$b += $e")
    //    q"$builder.state"
    //  }
    //}
  }


  object Query extends QueryFactory {
    override def Undefined: Query =
      Expr[basis.net.Query](q"_root_.basis.net.Query.Undefined")

    override def Part(query: String): Query =
      Expr[basis.net.Query](q"_root_.basis.net.Query.Part($query)")

    override def Param(key: String, value: String): QueryParam =
      Expr[(String, String)](q"($key, $value)")

    override def unapply(part: Part): Maybe[Query] =
      if (part.actualType <:< QueryTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Query]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, query :: Nil) =>
          basis.Bind(Expr[basis.net.Query](q"_root_.basis.net.Query($query)"))
        case _ => Trap
      }
      else Trap

    override def Builder: Builder[QueryParam] with State[Query] = new QueryExprBuilder()
  }

  private final class QueryExprBuilder extends Builder[QueryParam] with State[Query] {
    private[this] val self = mutable.ArrayBuffer.empty[QueryParam]
    override def append(param: QueryParam): Unit = self.append(param)
    override def clear(): Unit = self.clear()
    override def state: Query = Expr[basis.net.Query] {
      if (self.isEmpty) q"_root_.basis.net.Query.empty"
      else {
        val builder = self.foldLeft(q"_root_.basis.net.Query.Builder": Tree)((b, e) => q"$b += $e")
        q"$builder.state"
      }
    }
  }


  object Fragment extends FragmentFactory {
    override def Undefined: Fragment =
      Expr[basis.net.Fragment](q"_root_.basis.net.Fragment.Undefined")

    override def Part(fragment: String): Fragment =
      Expr[basis.net.Fragment](q"_root_.basis.net.Fragment.Part($fragment)")

    override def unapply(part: Part): Maybe[Fragment] =
      if (part.actualType <:< FragmentTag.tpe)
        basis.Bind(part.asInstanceOf[Expr[basis.net.Fragment]])
      else if (part.actualType <:< PartTag.tpe) part.tree match {
        case Apply(_, fragment :: Nil) =>
          basis.Bind(Expr[basis.net.Fragment](q"_root_.basis.net.Fragment($fragment)"))
        case _ => Trap
      }
      else Trap
  }
}
