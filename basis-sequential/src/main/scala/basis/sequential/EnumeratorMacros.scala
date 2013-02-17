/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

/** Enumerator operations macro implementations.
  * 
  * @author Chris Sachs
  */
private[sequential] final class EnumeratorMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def :+ [A]
      (these: Expr[Enumerator[A]], elem: Expr[A])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Select(
        Apply(
          Select(
            Apply(
              Select(
                builder.tree,
                ("++=": TermName).encodedName),
              these.tree :: Nil),
            ("+=": TermName).encodedName),
          elem.tree :: Nil),
        "state": TermName))
  }
  
  def +: [A]
      (elem: Expr[A], these: Expr[Enumerator[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Select(
        Apply(
          Select(
            Apply(
              Select(
                builder.tree,
                ("+=": TermName).encodedName),
              elem.tree :: Nil),
            ("++=": TermName).encodedName),
          these.tree :: Nil),
        "state": TermName))
  }
  
  def ++ [A]
      (these: Expr[Enumerator[A]], those: Expr[Enumerator[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Select(
        Apply(
          Select(
            Apply(
              Select(
                builder.tree,
                ("++=": TermName).encodedName),
              these.tree :: Nil),
            ("++=": TermName).encodedName),
          those.tree :: Nil),
        "state": TermName))
  }
  
  protected def BuilderTypeTag(builder: Expr[Builder[_]]): WeakTypeTag[builder.value.type] =
    WeakTypeTag[builder.value.type](builder.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => builder.actualType
    })
  
  protected def BuilderStateTag
      (builder: Expr[Builder[_]])
      (implicit BuilderTypeTag: WeakTypeTag[builder.value.type])
    : WeakTypeTag[builder.value.State] = {
    val BuilderTpc = mirror.staticClass("basis.collections.Builder").toType
    val BuilderStateSym = BuilderTpc member newTypeName("State")
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }
}
