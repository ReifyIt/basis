/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Random data generation. */
package object random {
  private[this] val localEntropy = new java.lang.ThreadLocal[Entropy]
  
  /** Returns a thread-local pseudorandom number generator. */
  implicit def Entropy: Entropy = {
    var S = localEntropy.get
    if (S == null) {
      S = new MersenneTwister32
      localEntropy.set(S)
    }
    S
  }
}
