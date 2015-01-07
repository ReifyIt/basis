//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import scala.reflect.macros._

private[generic] class FactoryMacros(val c: blackbox.Context) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def apply[CC[_], A](elems: Expr[A]*)(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    val n = q"${elems.length}"
    var b = q"$prefix.Builder[$A].expect($n)"

    val xs = elems.iterator
    while (xs.hasNext) b = q"$b += ${xs.next()}"

    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](q"$b.state")
  }

  def fill[CC[_], A](count: Expr[Int])(elem: Expr[A])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](q"""{
      var i = $count
      val b = $prefix.Builder[$A].expect(i)
      while (i > 0) {
        b.append($elem)
        i -= 1
      }
      b.state
    }""")
  }

  def iterate[CC[_], A](start: Expr[A], count: Expr[Int])(f: Expr[A => A])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](q"""{
      var i = $count
      val b = $prefix.Builder[$A].expect(i)
      if (i > 0) {
        var x = $start
        b.append(x)
        i -= 1
        while (i > 0) {
          x = $f(x)
          b.append(x)
          i -= 1
        }
      }
      b.state
    }""")
  }

  def tabulate[CC[_], A](count: Expr[Int])(f: Expr[Int => A])(implicit CC: WeakTypeTag[CC[_]], A: WeakTypeTag[A]): Expr[CC[A]] = {
    implicit val CCA = WeakTypeTag[CC[A]](appliedType(CC.tpe, A.tpe :: Nil))
    Expr[CC[A]](q"""{
      var i = 0
      val n = $count
      val b = $prefix.Builder[$A].expect(n)
      while (i < n) {
        b.append($f(i))
        i += 1
      }
      b.state
    }""")
  }
}
