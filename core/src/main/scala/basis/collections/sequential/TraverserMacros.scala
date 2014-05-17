//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

private[sequential] abstract class TraverserMacros(override val c: blackbox.Context) extends CollectionMacros(c) {
  import c.Expr
  import c.universe.{ Traverser => _, _ }

  def these: Expr[Traverser[_]]

  def :+ [A](elem: Expr[A])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      (($builder: $builderType) ++= xs += $elem).state
    }""")
  }

  def +: [A](elem: Expr[A])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      (($builder: $builderType) += $elem ++= xs).state
    }""")
  }

  def ++ [A](those: Expr[Traverser[A]])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val ys = $those
      (($builder: $builderType) ++= xs ++= ys).state
    }""")
  }
}
