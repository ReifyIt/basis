/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** Operations on [[Truth]] values.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Conditional
  * 
  * @groupprio  Evaluating  1
  */
final class TruthOps(truth: Truth) {
  /** Returns `true` if this is the `True` [[Truth]] value.
    * @group Evaluating */
  def isTrue: Boolean = macro TruthMacros.isTrue
  
  /** Returns `true` if this is the `False` [[Truth]] value.
    * @group Evaluating */
  def isFalse: Boolean = macro TruthMacros.isFalse
}

private[control] object TruthMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply(c: Context): c.Expr[Truth] = {
    import c.{Expr, mirror, prefix, typeCheck, WeakTypeTag}
    import c.universe._
    val Apply(_, truth :: Nil) = prefix.tree
    val BasisControlTpe = mirror.staticPackage("basis.control").moduleClass.typeSignature
    val TruthTpe = BasisControlTpe.member(newTypeName("Truth")).asType.toType
    Expr[Truth](typeCheck(truth, TruthTpe))(WeakTypeTag(TruthTpe))
  }
  
  def TruthOps(c: Context)(self: c.Expr[Truth]): c.Expr[TruthOps] = {
    import c.{Expr, mirror, WeakTypeTag}
    import c.universe._
    val TruthOpsTpe = mirror.staticClass("basis.control.TruthOps").toType
    Expr[TruthOps](New(TruthOpsTpe, self.tree))(WeakTypeTag(TruthOpsTpe))
  }
  
  def isTrue(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "True"), "eq"),
        unApply(c).tree :: Nil))
  }
  
  def isFalse(c: Context): c.Expr[Boolean] = {
    import c.Expr
    import c.universe._
    Expr[Boolean](
      Apply(
        Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "False"), "eq"),
        unApply(c).tree :: Nil))
  }
}
