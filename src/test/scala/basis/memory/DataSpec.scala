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
  override def suiteName = "Data specification"
  
  describe("Big-endian Byte Data") {
    implicit val allocator = ByteDataBE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian Byte Data") {
    implicit val allocator = ByteDataLE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian Short Data") {
    implicit val allocator = ShortDataBE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian Short Data") {
    implicit val allocator = ShortDataLE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian Int Data") {
    implicit val allocator = IntDataBE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian Int Data") {
    implicit val allocator = IntDataLE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian Long Data") {
    implicit val allocator = LongDataBE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian Long Data") {
    implicit val allocator = LongDataLE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian Buffer Data") {
    implicit val allocator = BufferDataBE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian Buffer Data") {
    implicit val allocator = BufferDataLE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
}
