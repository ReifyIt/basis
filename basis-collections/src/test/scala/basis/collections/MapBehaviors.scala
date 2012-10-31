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
  
  def genericMap[CC[A, T] <: Map[A, T]](CC: MapFactory[CC]) {
    describe(s"An empty associative $CC") {
      it("should known that it's empty") {
        val xs = CC[Any, Any]()
        xs.isEmpty should be (true)
      }
      
      it("should have size zero") {
        val xs = CC[Any, Any]()
        xs.size should be (0)
      }
    }
    
    describe(s"A unary associative $CC") {
      it("should know that it's non-empty") {
        val xs = CC("" -> 0)
        xs.isEmpty should be (false)
      }
      
      it("should have unit size") {
        val xs = CC("" -> 0)
        xs.size should be (1)
      }
      
      it("should contain its key") {
        val xs = CC("" -> 0)
        (xs contains "") should be (true)
      }
    }
    
    describe(s"A non-empty associative $CC") {
      it("should know its size") {
        val xs = CC(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
        xs.size should be (4)
      }
      
      it("should contain its keys") {
        val xs = CC(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
        withClue("key 1") ((xs contains 1) should be (true))
        withClue("key 2") ((xs contains 2) should be (true))
        withClue("key 3") ((xs contains 3) should be (true))
        withClue("key 4") ((xs contains 4) should be (true))
      }
      
      it("should not contain any non-keys") {
        val xs = CC(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
        withClue("key 0") ((xs contains 0) should be (false))
        withClue("key 5") ((xs contains 5) should be (false))
      }
    }
  }
}
