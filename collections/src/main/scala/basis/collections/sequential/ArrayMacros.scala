//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

private[sequential] abstract class ArrayMacros(override val c: blackbox.Context) extends ArrayLikeMacros(c) {
  import c.{ Expr, WeakTypeTag }
  import c.universe._

  override def these: Expr[Array[_]]

  def isEmpty[A]: Expr[Boolean] = Expr[Boolean](q"$these.length == 0")

  def zip[A, B](those: Expr[Array[B]])(builder: Expr[Builder[(A, B)]])(implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val ys = $those
      var i = 0
      val n = _root_.java.lang.Math.min(xs.length, ys.length)
      val b = $builder.expect(n): $builderType
      while (i < n) {
        b.append((xs(i): $A, ys(i): $B))
        i += 1
      }
      b.state
    }""")
  }

  def :+ [A](elem: Expr[A])(builder: Expr[ArrayBuilder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      (($builder.expect(xs.length + 1): $builderType) ++= xs += $elem).state
    }""")
  }

  def +: [A](elem: Expr[A])(builder: Expr[ArrayBuilder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      (($builder.expect(1 + xs.length): $builderType) += $elem ++= xs).state
    }""")
  }

  def ++ [A](those: Expr[Array[A]])(builder: Expr[ArrayBuilder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val ys = $those
      (($builder.expect(xs.length + ys.length): $builderType) ++= xs ++= ys).state
    }""")
  }
}
