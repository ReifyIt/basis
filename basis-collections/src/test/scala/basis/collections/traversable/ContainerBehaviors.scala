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

trait ContainerBehaviors extends CollectionBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def GenericContainer[CC[A] <: Container[A]](CC: BuilderFactory[CC]) {
    describe(s"An empty $CC container's iterator") {
      it("should be empty") {
        val xs = CC[Any]()
        xs.iterator.isEmpty should be (true)
      }
      
      it("should have no head") {
        val xs = CC[Any]()
        evaluating(xs.iterator.head) should produce [NoSuchElementException]
      }
      
      it("should not step") {
        val xs = CC[Any]()
        evaluating(xs.iterator.step()) should produce [UnsupportedOperationException]
      }
    }
    
    describe(s"A unary $CC container's iterator") {
       it("should step over its element") {
        val iter = CC(null).iterator
        withClue("1st state isEmpty") (iter.isEmpty should be (false))
        iter.step()
        withClue("2nd state isEmpty") (iter.isEmpty should be (true))
      }
      
      it("should visit its element") {
        val iter = CC("").iterator
        iter.head should be ("")
        iter.step()
      }
    }
    
    describe(s"A non-empty $CC container's iterator") {
      it("should step over its elements") {
        val iter = CC(1, 2, 3, 4).iterator
        withClue("1st state isEmpty") (iter.isEmpty should be (false))
        iter.step()
        withClue("2nd state isEmpty") (iter.isEmpty should be (false))
        iter.step()
        withClue("3rd state isEmpty") (iter.isEmpty should be (false))
        iter.step()
        withClue("4th state isEmpty") (iter.isEmpty should be (false))
        iter.step()
        withClue("5th state isEmpty") (iter.isEmpty should be (true))
      }
      
      it("should visit its elements once each (in any order)") {
        val iter = CC(2, 3, 5, 7).iterator
        var n = iter.head
        iter.step()
        n += iter.head
        iter.step()
        n += iter.head
        iter.step()
        n += iter.head
        iter.step()
        n should be (17)
      }
      
      it("should buffer its elements") {
        val iter = CC(2, 3, 5, 7).iterator
        withClue("1st state head") (iter.head should equal (iter.head))
        iter.step()
        withClue("2nd state head") (iter.head should equal (iter.head))
        iter.step()
        withClue("3rd state head") (iter.head should equal (iter.head))
        iter.step()
        withClue("4th state head") (iter.head should equal (iter.head))
        iter.step()
      }
    }
  }
}
