//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import org.scalatest._

class BeginSpec extends FlatSpec with Matchers {
  override def suiteName = "Breakable control-flow specification"

  "A breakable control-flow context" should "execute blocks" in {
    var x = 0
    def inc(): Unit = x += 1
    begin {
      var i = 0
      while (i < 10) {
        inc()
        i += 1
      }
    }
    x should equal (10)
  }

  it should "break out of blocks" in {
    var x = 0
    def inc(): Unit = if (x < 5) x += 1 else begin.break()
    begin {
      var i = 0
      while (i < 10) {
        inc()
        i += 1
      }
    }
    x should equal (5)
  }
}
