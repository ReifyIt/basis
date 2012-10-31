/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

import basis.collections.generic._
import basis.util._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait MapBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def GenericMap[CC[A, T] <: Map[A, T]](CC: MapFactory[CC]) {
    describe(s"An empty associative $CC") {
      it("should known that it's empty") {
        val xs = CC[Any, Any]()
        xs.isEmpty should be (true)
      }
      
      it("should have size zero") {
        val xs = CC[Any, Any]()
        xs.size should be (0)
      }
      
      it("should not traverse any entries") {
        val xs = CC[Any, Any]()
        traverse(xs)(x => fail())
      }
    }
    
    describe(s"A unary associative $CC") {
      it("should know that it's non-empty") {
        val xs = CC("zero" -> 0)
        xs.isEmpty should be (false)
      }
      
      it("should have unit size") {
        val xs = CC("zero" -> 0)
        xs.size should be (1)
      }
      
      it("should contain its key") {
        val xs = CC("zero" -> 0)
        (xs contains "zero") should be (true)
      }
      
      it("should traverse its entry once") {
        val xs = CC(2 -> 3)
        var k = 0
        var v = 0
        traverse(xs) { case (key, value) => k += key; v += value }
        k should be (2)
        v should be (3)
      }
    }
    
    describe(s"A non-empty associative $CC") {
      it("should know its size") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        xs.size should be (4)
      }
      
      it("should contain its keys") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("key \"one\"")   ((xs contains "one")   should be (true))
        withClue("key \"two\"")   ((xs contains "two")   should be (true))
        withClue("key \"three\"") ((xs contains "three") should be (true))
        withClue("key \"four\"")  ((xs contains "four")  should be (true))
      }
      
      it("should not contain non-keys") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("key \"zero\"") ((xs contains "zero") should be (false))
        withClue("key \"five\"") ((xs contains "five") should be (false))
      }
      
      it("should apply its keys") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("key \"one\"")   (xs("one")   should be (1))
        withClue("key \"two\"")   (xs("two")   should be (2))
        withClue("key \"three\"") (xs("three") should be (3))
        withClue("key \"four\"")  (xs("four")  should be (4))
      }
      
      it("should fail to apply non-keys") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("key \"zero\"") (evaluating(xs("zero")) should produce [NoSuchElementException])
        withClue("key \"five\"") (evaluating(xs("five")) should produce [NoSuchElementException])
      }
      
      it("should get its keys") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("key \"one\"")   (xs.get("one")   should be (Some(1)))
        withClue("key \"two\"")   (xs.get("two")   should be (Some(2)))
        withClue("key \"three\"") (xs.get("three") should be (Some(3)))
        withClue("key \"four\"")  (xs.get("four")  should be (Some(4)))
      }
      
      it("should not get non-keys") {
        val xs = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4)
        withClue("key \"zero\"")   (xs.get("zero") should be (None))
        withClue("key \"five\"")   (xs.get("five") should be (None))
      }
      
      it("should traverse its elements once each (in any order)") {
        val xs = CC(2 -> 3, 5 -> 7, 11 -> 13, 17 -> 19)
        var k = 0
        var v = 0
        traverse(xs) { case (key, value) => k += key; v += value }
        k should be (35)
        v should be (42)
      }
    }
    
    describe(s"An empty associative $CC's iterator") {
      it("should be empty") {
        val xs = CC[Any, Any]()
        xs.iterator.isEmpty should be (true)
      }
      
      it("should have no head") {
        val xs = CC[Any, Any]()
        evaluating(xs.iterator.head) should produce [NoSuchElementException]
      }
      
      it("should not step") {
        val xs = CC[Any, Any]()
        evaluating(xs.iterator.step()) should produce [UnsupportedOperationException]
      }
    }
    
    describe(s"A unary associative $CC's iterator") {
      it("should step over its entry") {
        val iter = CC("zero" -> 0).iterator
        withClue("1st state isEmpty") (iter.isEmpty should be (false))
        iter.step()
        withClue("2nd state isEmpty") (iter.isEmpty should be (true))
      }
      
      it("should visit its entry") {
        val iter = CC("zero" -> 0).iterator
        iter.head should be ("zero" -> 0)
        iter.step()
      }
    }
    
    describe(s"A non-empty associative $CC's iterator") {
      it("should step over its entries") {
        val iter = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4).iterator
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
      
      it("should visit its entries once each (in any order)") {
        val iter = CC(2 -> 3, 5 -> 7, 11 -> 13, 17 -> 19).iterator
        var k = 0
        var v = 0
        iter.head match { case (key, value) => k += key; v += value }
        iter.step()
        iter.head match { case (key, value) => k += key; v += value }
        iter.step()
        iter.head match { case (key, value) => k += key; v += value }
        iter.step()
        iter.head match { case (key, value) => k += key; v += value }
        iter.step()
        k should be (35)
        v should be (42)
      }
      
      it("should buffer its entries") {
        val iter = CC("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4).iterator
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
