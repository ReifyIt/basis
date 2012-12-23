/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait StructBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def ValueType[T](value: T)(implicit allocator: Allocator, struct: Struct[T]) {
    import allocator.alloc
    
    it("should store values") {
      val data = alloc[T](1L)
      struct.store(data, 0L, value)
      struct.load(data, 0L) should equal (value)
    }
    
    it("should store sequential values") {
      val data = alloc[T](3L)
      val address1 = 0L
      val address2 = address1 + struct.size
      val address3 = address2 + struct.size
      struct.store(data, address1, value)
      struct.store(data, address2, value)
      struct.store(data, address3, value)
      withClue("(1st value)") (struct.load(data, address1) should equal (value))
      withClue("(2nd value)") (struct.load(data, address2) should equal (value))
      withClue("(3rd value)") (struct.load(data, address3) should equal (value))
    }
  }
}
