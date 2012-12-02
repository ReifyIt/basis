/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package general

import basis.collections._
import basis.collections.generic._
import basis.collections.traversable._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait IndexedSeqOpsBehaviors extends SeqOpsBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def SequentialGeneralIndexedSeqOps[CC[A] <: IndexedSeq[A]](CC: BuilderFactory[CC]) {
    describe(s"A general indexed $CC sequence") {
      it("should traverse its elements once each with foreach") {
        val xs = CC(2, 3, 5, 7)
        var n = 0
        xs foreach (i => n += i)
        n should be (17)
      }
      
      it("should fold") {
        val xs = CC(2, 3, 5, 7)
        xs.fold(1)(_ + _) should be (18)
      }
      
      it("should fold when empty") {
        val xs = CC[Int]()
        xs.fold(-1)(_ + _) should be (-1)
      }
      
      it("should reduce") {
        val xs = CC(2, 3, 5, 7)
        xs.reduce(_ + _) should be (17)
      }
      
      it("should fail to reduce when empty") {
        val xs = CC[Int]()
        evaluating(xs.reduce(_ + _)) should produce [UnsupportedOperationException]
      }
      
      it("should optionally reduce") {
        val xs = CC(2, 3, 5, 7)
        xs.reduceOption(_ + _) should be (Some(17))
      }
      
      it("should optionally reduce to None when empty") {
        val xs = CC[Int]()
        xs.reduceOption(_ + _) should be (None)
      }
      
      it("should fold left") {
        val xs = CC(2, 3, 5, 7)
        xs.foldLeft(0.5)(_ + _) should be (17.5)
      }
      
      it("should fold left when empty") {
        val xs = CC[Int]()
        xs.foldLeft(0.5)(_ + _) should be (0.5)
      }
      
      it("should reduce left") {
        val xs = CC(2, 3, 5, 7)
        xs.reduceLeft(_ + _) should be (17)
      }
      
      it("should fail to reduce left when empty") {
        val xs = CC[Int]()
        evaluating(xs.reduceLeft(_ + _)) should produce [UnsupportedOperationException]
      }
      
      it("should optionally reduce left") {
        val xs = CC(2, 3, 5, 7)
        xs.reduceLeftOption(_ + _) should be (Some(17))
      }
      
      it("should optionally reduce left to None when empty") {
        val xs = CC[Int]()
        xs.reduceLeftOption(_ + _) should be (None)
      }
      
      it("should find an element when one satisfies a predicate") {
        val xs = CC(1, 3, 5, 8, 13)
        xs.find(i => i % 2 == 0) should be (Some(8))
      }
      
      it("should not find an element when none satisfy a predicate") {
        val xs = CC(2, 4, 6, 8)
        xs.find(i => i % 2 != 0) should be (None)
      }
      
      it("should verify that all elements satisfy a predicate") {
        val xs = CC(2, 4, 6, 8)
        xs.forall(i => i % 2 == 0) should be (true)
      }
      
      it("should verify that not all elements satisfy a predicate") {
        val xs = CC(2, 4, 5, 8)
        xs.forall(i => i % 2 == 0) should be (false)
      }
      
      it("should verify all preficates when empty") {
        val xs = CC[Any]()
        xs.forall(x => false) should be (true)
      }
      
      it("should verify there exists some element that satisfies a predicate") {
        val xs = CC(2, 4, 5, 8)
        xs.exists(i => i % 2 != 0) should be (true)
      }
      
      it("should verify there exists no element that satisfies a predicate") {
        val xs = CC(1, 3, 5, 7)
        xs.exists(i => i % 2 == 0) should be (false)
      }
      
      it("should disprove all predicates when empty") {
        val xs = CC[Any]()
        xs.exists(x => true) should be (false)
      }
      
      it("should count only elements that satisfy a predicate") {
        val xs = CC(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
        xs.count(i => i % 2 == 0) should be (5)
      }
      
      it("should choose an element defined for a partial function") {
        val xs = CC(2, 4, 5, 8)
        xs.choose {
          case 5 => "five"
        } should be (Some("five"))
      }
      
      it("should choose None when no element is defined for a partial function") {
        val xs = CC(1, 2, 3, 4)
        xs.choose {
          case 0 => "zero"
        } should be (None)
      }
    }
  }
}
