/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

private[sequential] final class EnumeratorMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def ++ [A]
      (these: Expr[Enumerator[A]], those: Expr[Enumerator[A]])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    Expr {
      Select(
        Apply(
          Select(
            Apply(
              Select(
                builder.tree,
                "$plus$plus$eq"),
              these.tree :: Nil),
            "$plus$plus$eq"),
          those.tree :: Nil),
        "state")
    } (StateTag(builder))
  }
  
  private def BuilderType(builder: Expr[Builder[_, _]]): Type = builder.tree.symbol match {
    case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
    case _ => builder.actualType
  }
  
  private def StateTag(builder: Expr[Builder[_, _]]): WeakTypeTag[builder.value.State] = {
    val StateSymbol = mirror.staticClass("basis.collections.Builder").toType.member(newTypeName("State"))
    WeakTypeTag(typeRef(BuilderType(builder), StateSymbol, Nil))
  }
}
