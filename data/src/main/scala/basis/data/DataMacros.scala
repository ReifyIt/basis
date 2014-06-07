//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import scala.reflect.macros._

private[data] class DataMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe._
  import c.universe.internal._

  def DataFactoryToOps[Data](factory: Expr[DataFactory[Data]])(implicit Data: WeakTypeTag[Data]): Expr[DataFactoryOps[Data]] = {
    implicit val DataFactoryOpsTag = WeakTypeTag[DataFactory[Data]](appliedType(mirror.staticClass("basis.data.DataFactoryOps").toTypeConstructor, Data.tpe :: Nil))
    Expr[DataFactoryOps[Data]](q"new $DataFactoryOpsTag($factory)")
  }

  def LoaderToOps(data: Expr[Loader]): Expr[LoaderOps[data.value.Family]] = {
    implicit val Family: WeakTypeTag[data.value.Family] = FamilyTag(data)
    implicit val LoaderOpsTag = WeakTypeTag[LoaderOps[data.value.Family]](appliedType(mirror.staticClass("basis.data.LoaderOps").toTypeConstructor, Family.tpe :: Nil))
    Expr[LoaderOps[data.value.Family]](q"new _root_.basis.data.LoaderOps($data)")
  }

  def ReaderToOps(data: Expr[Reader]): Expr[ReaderOps] = Expr[ReaderOps](q"new _root_.basis.data.ReaderOps($data)")
  def StorerToOps(data: Expr[Storer]): Expr[StorerOps] = Expr[StorerOps](q"new _root_.basis.data.StorerOps($data)")
  def WriterToOps(data: Expr[Writer]): Expr[WriterOps] = Expr[WriterOps](q"new _root_.basis.data.WriterOps($data)")

  implicit protected def ReaderOpsTag: WeakTypeTag[ReaderOps] = WeakTypeTag(mirror.staticClass("basis.data.ReaderOps").toType)
  implicit protected def StorerOpsTag: WeakTypeTag[StorerOps] = WeakTypeTag(mirror.staticClass("basis.data.StorerOps").toType)
  implicit protected def WriterOpsTag: WeakTypeTag[WriterOps] = WeakTypeTag(mirror.staticClass("basis.data.WriterOps").toType)

  protected def FamilyTag(xs: Expr[Family[_]]): WeakTypeTag[xs.value.Family] =
    WeakTypeTag(typeRef(StableType(xs), mirror.staticClass("basis.Family").toType.member(TypeName("Family")), Nil).dealias)

  protected def StableType(expr: Expr[_]): Type = expr.tree.symbol match {
    case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
    case _ => if (expr.actualType != null) expr.actualType else expr.staticType
  }
}
