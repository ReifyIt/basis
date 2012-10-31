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

trait CollectionBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def genericCollection[CC[A] <: Collection[A]](CC: BuilderFactory[CC]) {
    describe(s"An empty $CC collection") {
      it("should not traverse any elements") {
        val xs = CC[Any]()
        traverse(xs)(x => fail())
      }
    }
    
    describe(s"A non-empty $CC collection") {
      it("should traverse a single element once") {
        val xs = CC(null)
        var i = 0
        traverse(xs)(x => i += 1)
        i should be (1)
      }
      
      it("should traverse multiple elements once each (in any order)") {
        val xs = CC(2, 3, 5, 7)
        var i = 0
        traverse(xs)(x => i += x)
        i should be (17)
      }
    }
  }
}
