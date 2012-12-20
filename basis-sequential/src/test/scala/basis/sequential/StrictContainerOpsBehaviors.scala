/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait StrictContainerOpsBehaviors extends StrictCollectionOpsBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def SequentialStrictContainerOps[CC[A] <: Container[A]](CC: BuilderFactory[CC]) {
    import CC.Builder
    import strict._
    
    describe(s"A strict $CC container") {
      it("should collect all elements defined by a partial function") {
        val xs = CC(2, 3, 4, 5, 6, 7)
        val ys = xs.collect { case x if x % 2 != 0 => x.toDouble }
        ys.reduce(_ + _) should be (15.0)
      }
      
      it("should map elements") {
        val xs = CC(2, 3, 5, 7)
        val ys = xs map (_.toDouble)
        ys.reduce(_ + _) should be (17.0)
      }
      
      it("should flatMap elements") {
        val xs = CC(3, 11, 17, 29)
        val ys = xs flatMap (i => CC(i, i + 2))
        ys.reduce(_ + _) should be (128)
      }
      
      it("should filter out elements that do not satisfy a predicate") {
        val xs = CC(3, 4, 5, 6, 7, 8)
        val ys = xs filter (i => i % 2 != 0)
        ys.reduce(_ + _) should be (15)
      }
      
      it("should concatenate with another container") {
        val xs = CC(2, 3) ++ CC(5, 7)
        xs.reduce(_ + _) should be (17)
      }
    }
  }
}
