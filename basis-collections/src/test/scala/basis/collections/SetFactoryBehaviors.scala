/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait SetFactoryBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def GenericSetFactory[CC[A] <: Set[A]](CC: SetFactory[CC]) {
    describe(s"The unique $CC factory") {
      it("should apply zero elements") {
        val xs = CC[Any]()
        withClue("isEmpty") (xs.isEmpty should be (true))
        withClue("size") (xs.size should be (0))
      }
      
      it("should apply a single element") {
        val xs = CC("")
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("size") (xs.size should be (1))
      }
      
      it("should apply a fixed number of elements") {
        val xs = CC(1, 2, 3, 4)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("size") (xs.size should be (4))
      }
    }
  }
}
