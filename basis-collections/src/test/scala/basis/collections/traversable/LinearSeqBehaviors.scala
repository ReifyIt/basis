/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait LinearSeqBehaviors extends SeqBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def GenericLinearSeq[CC[X] <: LinearSeq[X]](CC: BuilderFactory[CC]) {
    describe(s"An empty linear $CC sequence") {
      it("should have no head") {
        val xs = CC[Any]()
        evaluating(xs.head) should produce [NoSuchElementException]
      }
      
      it("should have no tail") {
        val xs = CC[Any]()
        evaluating(xs.tail) should produce [UnsupportedOperationException]
      }
    }
    
    describe(s"A unit-length linear $CC sequence") {
      it("should have a head") {
        val xs = CC("")
        xs.head should be ("")
      }
      
      it("should have an empty tail") {
        val xs = CC("")
        xs.tail.isEmpty should be (true)
      }
    }
    
    describe(s"A non-empty linear $CC sequence") {
      it("should list elements in-order") {
        var xs: LinearSeq[Int] = CC(1, 2, 3, 4)
        withClue("1st cell isEmpty") (xs.isEmpty should be (false))
        withClue("1st head") (xs.head should equal (1))
        xs = xs.tail
        withClue("2nd cell isEmpty") (xs.isEmpty should be (false))
        withClue("2nd head") (xs.head should equal (2))
        xs = xs.tail
        withClue("3rd cell isEmpty") (xs.isEmpty should be (false))
        withClue("3rd head") (xs.head should equal (3))
        xs = xs.tail
        withClue("4th cell isEmpty") (xs.isEmpty should be (false))
        withClue("4th head") (xs.head should equal (4))
        xs = xs.tail
        withClue("5th cell isEmpty") (xs.isEmpty should be (true))
      }
    }
  }
}
