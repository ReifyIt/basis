/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

import basis.collections.generic._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait SeqBehaviors extends ContainerBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def TraversableSeq[CC[A] <: Seq[A]](CC: BuilderFactory[CC]) {
    describe(s"An empty $CC sequence") {
      it("should know that it's empty") {
        val xs = CC[Any]()
        xs.isEmpty should be (true)
      }
      
      it("should have length zero") {
        val xs = CC[Any]()
        xs.length should be (0)
      }
    }
    
    describe(s"A non-empty $CC sequence") {
      it("should know that it's non-empty") {
        val xs = CC(null)
        xs.isEmpty should be (false)
      }
      
      it("should know its length") {
        val xs = CC(1, 2, 3, 4)
        xs.length should be (4)
      }
      
      it("should traverse elements in-order") {
        val xs = CC(1, 2, 3, 4)
        var i = 1
        traverse(xs) {
          case 1 =>
            withClue("1st element") (i should be (1))
            i = 2
          case 2 =>
            withClue("2nd element") (i should be (2))
            i = 3
          case 3 =>
            withClue("3rd element") (i should be (3))
            i = 4
          case 4 =>
            withClue("4th element") (i should be (4))
            i = 5
          case _ => fail()
        }
        withClue("final state") (i should be (5))
      }
      
      it("should iterate over elements in-order") {
        val iter = CC(1, 2, 3, 4).iterator
        withClue("1st element") (iter.head should equal (1))
        iter.step()
        withClue("2nd element") (iter.head should equal (2))
        iter.step()
        withClue("3rd element") (iter.head should equal (3))
        iter.step()
        withClue("4th element") (iter.head should equal (4))
        iter.step()
      }
    }
  }
}
