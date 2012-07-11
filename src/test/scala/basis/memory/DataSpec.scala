/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class DataSpec extends FunSpec with ShouldMatchers with DataBehaviors {
  import Data._
  
  override def suiteName = "Data specification"
  
  describe("Big-endian base-1 addressed data") {
    implicit val allocator = Base1BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-1 addressed data") {
    implicit val allocator = Base1LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian base-2 addressed data") {
    implicit val allocator = Base2BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-2 addressed data") {
    implicit val allocator = Base2LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian base-4 addressed data") {
    implicit val allocator = Base4BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-4 addressed data") {
    implicit val allocator = Base4LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian base-8 addressed data") {
    implicit val allocator = Base8BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-8 addressed data") {
    implicit val allocator = Base8LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian buffer data") {
    implicit val allocator = BufferBE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian buffer data") {
    implicit val allocator = BufferLE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
}
