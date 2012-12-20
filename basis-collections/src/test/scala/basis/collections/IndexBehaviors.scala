/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait IndexBehaviors extends SeqBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def TraversableIndex[CC[X] <: Index[X]](CC: BuilderFactory[CC]) {
    describe(s"An empty $CC index") {
      it("should have length zero") {
        val xs = CC[Any]()
        xs.length should equal (0)
      }
    }
    
    describe(s"A non-empty $CC index") {
      it("should have the correct length") {
        val xs = CC(1, 2, 3, 4)
        xs.length should equal (4)
      }
      
      it("should access elements by index") {
        val xs = CC(11, 13, 17, 19)
        withClue("index 0:") (xs(0) should equal (11))
        withClue("index 1:") (xs(1) should equal (13))
        withClue("index 2:") (xs(2) should equal (17))
        withClue("index 3:") (xs(3) should equal (19))
      }
    }
  }
}
