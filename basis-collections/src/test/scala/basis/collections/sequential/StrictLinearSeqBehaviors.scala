/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.traversable._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait StrictLinearSeqBehaviors extends StrictSeqBehaviors { this: FunSpec =>
  import strict.{GeneralLinearSeqOps, StrictLinearSeqOps}
  import ShouldMatchers._
  
  def GenericStrictLinearSeq[CC[A] <: LinearSeq[A]](CC: BuilderFactory[CC]) {
    import CC.Builder
    
    describe(s"A strict linear $CC sequence") {
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
      
      it("should drop elements while a predicate holds true") {
        val xs = CC(0, 1, 2, 3, 5, 7)
        val ys = xs dropWhile(i => i <= 2)
        ys.reduce(_ + _) should be (15)
      }
      
      it("should take elements while a predicate holds true") {
        val xs = CC(3, 5, 7, -1, -2, -3)
        val ys = xs takeWhile(i => i > 0)
        ys.reduce(_ + _) should be (15)
      }
      
      it("should span the longest prefix for which a predicate holds true") (pending)
      
      it("should drop elements") {
        val xs = CC(2, 3, 5, 7)
        val ys = xs drop 2
        ys.reduce(_ + _) should be (12)
      }
      
      it("should take elements") {
        val xs = CC(2, 3, 5, 7)
        val ys = xs take 2
        ys.reduce(_ + _) should be (5)
      }
      
      it("should slice an interval") {
        val xs = CC(2, 3, 5, 7)
        val ys = xs.slice(1, 3)
        ys.reduce(_ + _) should be (8)
      }
      
      it("should zip with itself") {
        val xs = CC(2, 3, 5, 7)
        val ys = xs zip xs
        for ((i, j) <- ys) i should equal (j)
      }
      
      it("should concatenate with another linear sequence") {
        val xs = CC(2, 3) ++ CC(5, 7)
        xs.reduce(_ + _) should be (17)
      }
    }
  }
}
