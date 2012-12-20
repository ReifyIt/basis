/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait StackBehaviors extends SeqBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def TraversableStack[CC[X] <: Stack[X]](CC: BuilderFactory[CC]) {
    describe(s"An empty $CC stack") {
      it("should have no head") {
        val xs = CC[Any]()
        evaluating(xs.head) should produce [NoSuchElementException]
      }
      
      it("should have no tail") {
        val xs = CC[Any]()
        evaluating(xs.tail) should produce [UnsupportedOperationException]
      }
    }
    
    describe(s"A unit-length $CC stack") {
      it("should have a head") {
        val xs = CC("")
        xs.head should be ("")
      }
      
      it("should have an empty tail") {
        val xs = CC("")
        xs.tail.isEmpty should be (true)
      }
    }
    
    describe(s"A non-empty $CC stack") {
      it("should list elements in-order") {
        var xs: Stack[Int] = CC(1, 2, 3, 4)
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
