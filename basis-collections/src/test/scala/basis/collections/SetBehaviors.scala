/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait SetBehaviors extends ContainerBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def TraversableSet[CC[A] <: Set[A]](CC: BuilderFactory[CC]) {
    describe(s"An empty unique $CC") {
      it("should known that it's empty") {
        val xs = CC[Any]()
        xs.isEmpty should be (true)
      }
      
      it("should have size zero") {
        val xs = CC[Any]()
        xs.size should be (0)
      }
    }
    
    describe(s"A unary unique $CC") {
      it("should know that it's non-empty") {
        val xs = CC("")
        xs.isEmpty should be (false)
      }
      
      it("should have unit size") {
        val xs = CC("")
        xs.size should be (1)
      }
      
      it("should contain its element") {
        val xs = CC("")
        (xs contains "") should be (true)
      }
    }
    
    describe(s"A non-empty unique $CC") {
      it("should know its size") {
        val xs = CC(1, 2, 3, 4)
        xs.size should be (4)
      }
      
      it("should contain its elements") {
        val xs = CC(1, 2, 3, 4)
        withClue("element 1") ((xs contains 1) should be (true))
        withClue("element 2") ((xs contains 2) should be (true))
        withClue("element 3") ((xs contains 3) should be (true))
        withClue("element 4") ((xs contains 4) should be (true))
      }
      
      it("should not contain any non-elements") {
        val xs = CC(1, 2, 3, 4)
        withClue("element 0") ((xs contains 0) should be (false))
        withClue("element 5") ((xs contains 5) should be (false))
      }
      
      it("should not contain duplicate elements") {
        val iter = CC(2, 2, 3, 5, 5, 5, 7, 7).iterator
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
    }
  }
}
