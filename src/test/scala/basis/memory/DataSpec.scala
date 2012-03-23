/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class DataSpec
  extends FunSpec
    with ShouldMatchers
    with DataBehaviors {
  
  override def suiteName = "Data Specification"
  
  describe("Big-endian Block1 Data") {
    implicit val allocator = Data.Block1BE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like bigEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Little-endian Block1 Data") {
    implicit val allocator = Data.Block1LE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like littleEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Big-endian Block2 Data") {
    implicit val allocator = Data.Block2BE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like bigEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Little-endian Block2 Data") {
    implicit val allocator = Data.Block2LE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like littleEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Big-endian Block4 Data") {
    implicit val allocator = Data.Block4BE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like bigEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Little-endian Block4 Data") {
    implicit val allocator = Data.Block4LE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like littleEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Big-endian Block8 Data") {
    implicit val allocator = Data.Block8BE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like bigEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Little-endian Block8 Data") {
    implicit val allocator = Data.Block8LE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like littleEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Big-endian Chunk Data") {
    implicit val allocator = Data.ChunkBE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like bigEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
  
  describe("Little-endian Chunk Data") {
    implicit val allocator = Data.ChunkLE
    
    it should behave like primitiveValueStore(allocator)
    
    it should behave like littleEndianValueStore(allocator)
    
    it should behave like primitiveStructStore(allocator)
  }
}
