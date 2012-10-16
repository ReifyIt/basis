/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait ValTypeBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def ValueType[T](value: T)(implicit allocator: Allocator, struct: ValType[T]) {
    it("should store values") {
      val mem = Mem.alloc[T](1L)
      struct.store(mem, 0L, value)
      struct.load(mem, 0L) should equal (value)
    }
    
    it("should store sequential values") {
      val mem = Mem.alloc[T](3L)
      val address1 = 0L
      val address2 = address1 + struct.size
      val address3 = address2 + struct.size
      struct.store(mem, address1, value)
      struct.store(mem, address2, value)
      struct.store(mem, address3, value)
      withClue("(1st value)") (struct.load(mem, address1) should equal (value))
      withClue("(2nd value)") (struct.load(mem, address2) should equal (value))
      withClue("(3rd value)") (struct.load(mem, address3) should equal (value))
    }
  }
}
