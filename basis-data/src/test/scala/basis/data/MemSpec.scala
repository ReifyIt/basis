/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class MemSpec extends FunSpec with ShouldMatchers with MemBehaviors {
  override def suiteName = "Memory specification"
  
  describe("Big-endian base-1 addressed memory") {
    implicit val allocator = Mem1BE
    it should behave like PrimitiveMemory(allocator)
    it should behave like BigEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Little-endian base-1 addressed memory") {
    implicit val allocator = Mem1LE
    it should behave like PrimitiveMemory(allocator)
    it should behave like LittleEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Big-endian base-2 addressed memory") {
    implicit val allocator = Mem2BE
    it should behave like PrimitiveMemory(allocator)
    it should behave like BigEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Little-endian base-2 addressed memory") {
    implicit val allocator = Mem2LE
    it should behave like PrimitiveMemory(allocator)
    it should behave like LittleEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Big-endian base-4 addressed memory") {
    implicit val allocator = Mem4BE
    it should behave like PrimitiveMemory(allocator)
    it should behave like BigEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Little-endian base-4 addressed memory") {
    implicit val allocator = Mem4LE
    it should behave like PrimitiveMemory(allocator)
    it should behave like LittleEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Big-endian base-8 addressed memory") {
    implicit val allocator = Mem8BE
    it should behave like PrimitiveMemory(allocator)
    it should behave like BigEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("Little-endian base-8 addressed memory") {
    implicit val allocator = Mem8LE
    it should behave like PrimitiveMemory(allocator)
    it should behave like LittleEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
  
  describe("memory buffer") {
    implicit val allocator = MemBuf
    it should behave like PrimitiveMemory(allocator)
    it should behave like NativeEndianMemory(allocator)
    it should behave like StructuredMemory(allocator)
  }
}
