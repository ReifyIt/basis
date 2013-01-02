/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait SeqFactoryBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def GenericSeqFactory[CC[A] <: Seq[A]](CC: SeqFactory[CC]) {
    describe(s"The $CC sequence factory") {
      it("should apply zero elements") {
        val xs = CC[Any]()
        withClue("isEmpty") (xs.isEmpty should be (true))
        withClue("length") (xs.length should be (0))
      }
      
      it("should apply a single element") {
        val xs = CC("")
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (1))
      }
      
      it("should apply a fixed number of elements") {
        val xs = CC(1, 2, 3, 4)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (4))
      }
      
      it("should fill zero elements") {
        val xs = CC.fill[Any](0)(fail())
        withClue("isEmpty") (xs.isEmpty should be (true))
        withClue("length") (xs.length should be (0))
      }
      
      it("should fill a single element") {
        val xs = CC.fill(1)("")
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (1))
      }
      
      it("should fill a small number of elements") {
        val xs = CC.fill(17)(0)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (17))
      }
      
      it("should fill a moderate number of elements") {
        val xs = CC.fill(4097)(0)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (4097))
      }
      
      it("should tabulate zero elements") {
        val xs = CC.tabulate[Any](0)(i => fail())
        withClue("isEmpty") (xs.isEmpty should be (true))
        withClue("length") (xs.length should be (0))
      }
      
      it("should tabulate a single element") {
        val xs = CC.tabulate(1)(i => i + 1)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (1))
      }
      
      it("should tabulate a small number of elements") {
        val xs = CC.tabulate(15)(i => i.toString)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (15))
      }
      
      it("should tabulate a moderate number of elements") {
        val xs = CC.tabulate(4095)(i => i.toFloat)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (4095))
      }
      
      it("should iterate some value zero times") {
        val xs = CC.iterate[Any]("", 0)(s => fail())
        withClue("isEmpty") (xs.isEmpty should be (true))
        withClue("length") (xs.length should be (0))
      }
      
      it("should iterate some value one time") {
        val xs = CC.iterate(0, 1)(i => i + 1)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (1))
      }
      
      it("should iterate some value a small number of times") {
        val xs = CC.iterate(1, 16)(n => 2 * n)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (16))
      }
      
      it("should iterate some value a moderate number of times") {
        val xs = CC.iterate(0, 4096)(i => i + 1)
        withClue("isEmpty") (xs.isEmpty should be (false))
        withClue("length") (xs.length should be (4096))
      }
    }
  }
}
