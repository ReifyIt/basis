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

trait CollectionBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def TraversableCollection[CC[A] <: Collection[A]](CC: generic.BuilderFactory[CC]) {
    describe(s"An empty $CC collection") {
      it("should not traverse any elements") {
        val xs = CC[Any]()
        traverse(xs)(x => fail())
      }
    }
    
    describe(s"A unary $CC collection") {
      it("should traverse its element once") {
        val xs = CC(null)
        var n = 0
        traverse(xs)(x => n += 1)
        n should be (1)
      }
    }
    
    describe(s"A non-empty $CC collection") {
      it("should traverse its elements once each (in any order)") {
        val xs = CC(2, 3, 5, 7)
        var n = 0
        traverse(xs)(i => n += i)
        n should be (17)
      }
    }
  }
}
