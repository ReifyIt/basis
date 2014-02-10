//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

final class TruthOps(val __ : Truth) extends AnyVal {
  /** Returns `true` if this is the `true` value of the `Truth`, otherwise returns `false`.
    * @group Evaluating */
  def isTrue: Boolean = macro TruthOps.isTrue

  /** Returns `true` if this is the `false` value of the `Truth`, otherwise returns `false`.
    * @group Evaluating */
  def isFalse: Boolean = macro TruthOps.isFalse
}

private[util] object TruthOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply(c: Context): c.Expr[Truth] = {
    import c.{ Expr, mirror, prefix, typeCheck, WeakTypeTag }
    import c.universe._
    val Apply(_, self :: Nil) = prefix.tree
    implicit val TruthTag =
      WeakTypeTag[Truth](
        mirror.staticPackage("basis.util").moduleClass.typeSignature.
          member("Truth": TypeName).asType.toType)
    Expr[Truth](typeCheck(self, weakTypeOf[Truth]))
  }

  def TruthToOps(c: Context)(self: c.Expr[Truth]): c.Expr[TruthOps] = {
    import c.{ Expr, mirror, WeakTypeTag }
    import c.universe._
    implicit val TruthOpsTag =
      WeakTypeTag[TruthOps](mirror.staticClass("basis.util.TruthOps").toType)
    Expr[TruthOps](
      Apply(
        Select(New(TypeTree(weakTypeOf[TruthOps])), nme.CONSTRUCTOR),
        self.tree :: Nil))
  }

  def isTrue(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(BasisUtil(c), "True": TermName), "eq": TermName),
        unApply(c).tree :: Nil))
  }

  def isFalse(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(BasisUtil(c), "False": TermName), "eq": TermName),
        unApply(c).tree :: Nil))
  }

  private def BasisUtil(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName)
  }
}
