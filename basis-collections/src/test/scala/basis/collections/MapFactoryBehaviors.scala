/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait MapFactoryBehaviors { this: FunSpec =>
  import ShouldMatchers._
  import basis.util.ArrowOps
  
  def GenericMapFactory[CC[A, T] <: Map[A, T]](CC: MapFactory[CC]) {
    describe(s"The associative $CC factory") {
      it("should apply zero entries") {
        val xs = CC[Any, Any]()
        withClue("isEmpty") (xs.isEmpty should be (true))
        withClue("size") (xs.size should be (0))
      }
      
      it("should apply a single entry") {
        val xs = CC("zero" -> 0)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("size") (xs.size should be (1))
      }
      
      it("should apply a fixed number of entries") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("size") (xs.size should be (4))
      }
    }
  }
}
