//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

package object stat {
  private[this] val localRandom = new java.lang.ThreadLocal[Random]

  /** Returns a thread-local pseudorandom number generator. */
  implicit def Random: Random = {
    var S = localRandom.get
    if (S == null) {
      S = new MersenneTwister32
      localRandom.set(S)
    }
    S
  }
}
