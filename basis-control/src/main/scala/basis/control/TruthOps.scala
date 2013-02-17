/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** Standard [[Truth]] operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Conditional
  * 
  * @groupprio  Evaluating  1
  */
final class TruthOps(truth: Truth) {
  /** Returns `true` if this is the `true` value of the `Truth`, otherwise returns `false`.
    * @group Evaluating */
  def isTrue: Boolean = macro TruthMacros.isTrue
  
  /** Returns `true` if this is the `false` value of the `Truth`, otherwise returns `false`.
    * @group Evaluating */
  def isFalse: Boolean = macro TruthMacros.isFalse
}

private[control] object TruthMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Truth] = {
    import c.{Expr, mirror, prefix, typeCheck, WeakTypeTag}
    import c.universe._
    val Apply(_, self :: Nil) = prefix.tree
    implicit val TruthTag =
      WeakTypeTag[Truth](
        mirror.staticPackage("basis.control").moduleClass.typeSignature.
          member(newTypeName("Truth")).asType.toType)
    Expr[Truth](typeCheck(self, weakTypeOf[Truth]))
  }
  
  def TruthOps(c: Context)(self: c.Expr[Truth]): c.Expr[TruthOps] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    implicit val TruthOpsTag =
      WeakTypeTag[TruthOps](mirror.staticClass("basis.control.TruthOps").toType)
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
        Select(Select(BasisControl(c), "True": TermName), "eq": TermName),
        unApply(c).tree :: Nil))
  }
  
  def isFalse(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(BasisControl(c), "False": TermName), "eq": TermName),
        unApply(c).tree :: Nil))
  }
  
  private def BasisControl(c: Context): c.Tree = {
    import c.universe._
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "control": TermName)
  }
}
