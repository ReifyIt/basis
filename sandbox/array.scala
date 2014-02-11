package p {
  import basis._
  import basis.collections._

  class Bippy {
    val xs: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val intFn: Int => Int = x => x
    val intFn2: (Int, Int) => Int = _ + _

    def f0 = xs.isEmpty
    def f1 = xs foreach intFn
    def f2 = xs.foldLeft(0)(intFn2)
    def f3 = xs reduceLeft intFn2
    def f4 = xs find (_ > 5)

    def g0 = xs mayReduceLeft intFn2
  }
}

package object p {
  val intArrayOps      = null
  val wrapIntArray     = null
  val genericArrayOps  = null
  val genericWrapArray = null
}

package q {
  class Dingo {
    val xs: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val intFn: Int => Int = x => x
    val intFn2: (Int, Int) => Int = _ + _

    def f0 = xs.isEmpty
    def f1 = xs foreach intFn
    def f2 = xs.foldLeft(0)(intFn2)
    def f3 = xs reduceLeft intFn2
    def f4 = xs find (_ > 5)

    def g0 = xs reduceLeftOption intFn2
  }
}

