//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

object CollectionGenerators {
  implicit class CollectionFactoryGenerators[CC[X] <: Collection[X]](val CC: generic.CollectionFactory[CC]) extends AnyVal {
    def range(lower: Int, upper: Int): CC[Int] = {
      val b = CC.Builder[Int]
      var i = lower
      while (i <= upper) {
        b.append(i)
        i += 1
      }
      b.state
    }
  }

  implicit class MapFactoryGenerators[CC[X, Y] <: Map[X, Y]](val CC: generic.MapFactory[CC]) extends AnyVal {
    def range(lower: Int, upper: Int): CC[Int, Int] = {
      val b = CC.Builder[Int, Int]
      var i = lower
      while (i <= upper) {
        b.append((i, i))
        i += 1
      }
      b.state
    }
  }
}
