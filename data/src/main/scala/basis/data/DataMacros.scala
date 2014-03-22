//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import scala.reflect.macros._

private[data] class DataMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe._

  def LoaderToOps(data: Expr[Loader]): Expr[LoaderOps] = Expr[LoaderOps](q"new _root_.basis.data.LoaderOps($data)")
  def ReaderToOps(data: Expr[Reader]): Expr[ReaderOps] = Expr[ReaderOps](q"new _root_.basis.data.ReaderOps($data)")
  def StorerToOps(data: Expr[Storer]): Expr[StorerOps] = Expr[StorerOps](q"new _root_.basis.data.StorerOps($data)")
  def WriterToOps(data: Expr[Writer]): Expr[WriterOps] = Expr[WriterOps](q"new _root_.basis.data.WriterOps($data)")

  implicit protected def LoaderOpsTag: WeakTypeTag[LoaderOps] = WeakTypeTag(mirror.staticClass("basis.data.LoaderOps").toType)
  implicit protected def ReaderOpsTag: WeakTypeTag[ReaderOps] = WeakTypeTag(mirror.staticClass("basis.data.ReaderOps").toType)
  implicit protected def StorerOpsTag: WeakTypeTag[StorerOps] = WeakTypeTag(mirror.staticClass("basis.data.StorerOps").toType)
  implicit protected def WriterOpsTag: WeakTypeTag[WriterOps] = WeakTypeTag(mirror.staticClass("basis.data.WriterOps").toType)
}
