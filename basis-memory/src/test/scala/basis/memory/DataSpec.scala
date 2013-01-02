/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class DataSpec extends FunSpec with ShouldMatchers with DataBehaviors {
  override def suiteName = "Data specification"
  
  describe("Big-endian base-1 addressed data") {
    implicit val allocator = Data1BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-1 addressed data") {
    implicit val allocator = Data1LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian base-2 addressed data") {
    implicit val allocator = Data2BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-2 addressed data") {
    implicit val allocator = Data2LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian base-4 addressed data") {
    implicit val allocator = Data4BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-4 addressed data") {
    implicit val allocator = Data4LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Big-endian base-8 addressed data") {
    implicit val allocator = Data8BE
    it should behave like PrimitiveData(allocator)
    it should behave like BigEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("Little-endian base-8 addressed data") {
    implicit val allocator = Data8LE
    it should behave like PrimitiveData(allocator)
    it should behave like LittleEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
  
  describe("ByteBuffer backed data") {
    implicit val allocator = DataBuffer
    it should behave like PrimitiveData(allocator)
    it should behave like NativeEndianData(allocator)
    it should behave like StructuredData(allocator)
  }
}
