//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis.util._

object CollectionGenerators {
  def FirstNIntegers[CC[X] <: Collection[X]](CC: generic.CollectionFactory[CC], n: Int): CC[Int] = {
    val b = CC.Builder[Int]
    var i = 1
    while (i <= n) {
      b.append(i)
      i += 1
    }
    b.state
  }

  def FirstNIntegers[CC[X, Y] <: Map[X, Y]](CC: generic.MapFactory[CC], n: Int): CC[Int, Int] = {
    val b = CC.Builder[Int, Int]
    var i = 1
    while (i <= n) {
      b.append(i -> i)
      i += 1
    }
    b.state
  }
}
