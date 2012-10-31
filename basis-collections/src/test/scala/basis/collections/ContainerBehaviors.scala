/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

import basis.collections.generic._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait ContainerBehaviors extends CollectionBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def genericContainer[CC[A] <: Container[A]](CC: BuilderFactory[CC]) {
    describe(s"An empty $CC container's iterator") {
      it("should be empty") {
        val xs = CC[Any]()
        xs.iterator.isEmpty should be (true)
      }
    }
    
    describe(s"A non-empty $CC container's iterator") {
      it("should step over a single element") {
        val iter = CC(null).iterator
        withClue("non-empty initially") (iter.isEmpty should be (false))
        iter.step()
        withClue("empty after first step") (iter.isEmpty should be (true))
      }
      
      it("should step over multiple elements") {
        val iter = CC(1, 2, 3, 4).iterator
        withClue("1st state") (iter.isEmpty should be (false))
        iter.step()
        withClue("2nd state") (iter.isEmpty should be (false))
        iter.step()
        withClue("3rd state") (iter.isEmpty should be (false))
        iter.step()
        withClue("4th state") (iter.isEmpty should be (false))
        iter.step()
        withClue("5th state") (iter.isEmpty should be (true))
      }
      
      it("should visit multiple elements once each (in any order)") {
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
      
      it("should buffer elements") {
        val iter = CC(2, 3, 5, 7).iterator
        withClue("1st state") (iter.head should equal (iter.head))
        iter.step()
        withClue("2nd state") (iter.head should equal (iter.head))
        iter.step()
        withClue("3rd state") (iter.head should equal (iter.head))
        iter.step()
        withClue("4th state") (iter.head should equal (iter.head))
        iter.step()
      }
    }
  }
}
