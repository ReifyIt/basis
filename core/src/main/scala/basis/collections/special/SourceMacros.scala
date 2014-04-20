//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

import scala.reflect.macros._

private[special] class SourceMacros(val c: blackbox.Context) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def apply[CC, A](elems: Expr[A]*)(implicit CC: WeakTypeTag[CC]): Expr[CC] = {
    val n = q"${elems.length}"
    var b = q"$prefix.Builder.expect($n)"

    val xs = elems.iterator
    while (xs.hasNext) b = q"$b += ${xs.next()}"

    Expr[CC](q"$b.state")
  }

  def fill[CC, A](count: Expr[Int])(elem: Expr[A])(implicit CC: WeakTypeTag[CC]): Expr[CC] = Expr[CC](q"""{
    var i = $count
    val b = $prefix.Builder.expect(i)
    while (i > 0) {
      b.append($elem)
      i -= 1
    }
    b.state
  }""")

  def iterate[CC, A](start: Expr[A], count: Expr[Int])(f: Expr[A => A])(implicit CC: WeakTypeTag[CC]): Expr[CC] = Expr[CC](q"""{
    var i = $count
    val b = $prefix.Builder.expect(i)
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

  def tabulate[CC, A](count: Expr[Int])(f: Expr[Int => A])(implicit CC: WeakTypeTag[CC]): Expr[CC] = Expr[CC](q"""{
    var i = 0
    val n = $count
    val b = $prefix.Builder.expect(n)
    while (i < n) {
      b.append($f(i))
      i += 1
    }
    b.state
  }""")
}
