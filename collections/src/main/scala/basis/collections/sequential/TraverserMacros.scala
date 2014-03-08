//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.collection.immutable.{ ::, Nil }
import scala.reflect.macros.Context

private[sequential] final class TraverserMacros[C <: Context](val context: C) {
  import context.{ Expr, fresh, mirror, WeakTypeTag }
  import universe.{ Traverser => _, _ }
  import internal._

  val universe: context.universe.type = context.universe

  def :+ [A]
      (these: Expr[Traverser[A]], elem: Expr[A])
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
      (elem: Expr[A], these: Expr[Traverser[A]])
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
      (these: Expr[Traverser[A]], those: Expr[Traverser[A]])
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
    val BuilderStateSym = BuilderTpc.member("State": TypeName)
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }
}
