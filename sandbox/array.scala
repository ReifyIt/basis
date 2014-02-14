package p {
  import basis._
  import basis.collections._

  final class Bippy {
    private[this] val xs: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    def f0 = xs.isEmpty
    def f1 = xs foreach (x => x)
    def f2 = xs.foldLeft(0)(_ + _)
    def f3 = xs reduceLeft (_ + _)
    def f4 = xs find (_ > 5)

    def g0 = xs mayReduceLeft (_ + _)

    override def toString = "" + ((f0, f1, f2, f3, f4, g0))
  }
}

package object p {
  val intArrayOps      = null
  val wrapIntArray     = null
  val genericArrayOps  = null
  val genericWrapArray = null
}

package q {
  final class Dingo {
    private[this] val xs: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    def f0 = xs.isEmpty
    def f1 = xs foreach (x => x)
    def f2 = xs.foldLeft(0)(_ + _)
    def f3 = xs reduceLeft (_ + _)
    def f4 = xs find (_ > 5)

    def g0 = xs reduceLeftOption (_ + _)

    override def toString = "" + ((f0, f1, f2, f3, f4, g0))
  }
}
