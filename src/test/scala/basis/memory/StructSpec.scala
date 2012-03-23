/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class StructSpec extends FunSpec with ShouldMatchers {
  override def suiteName = "Struct Specification"
  
  describe("Packed Byte Struct") {
    val struct = Struct.PackedByte
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 1") {
      struct.size should equal (1L)
    }
  }
  
  describe("Padded Short Struct") {
    val struct = Struct.PaddedShort
    
    it("should have alignment 2") {
      struct.alignment should equal (2L)
    }
    
    it("should have size 2") {
      struct.size should equal (2L)
    }
  }
  
  describe("Padded Int Struct") {
    val struct = Struct.PaddedInt
    
    it("should have alignment 4") {
      struct.alignment should equal (4L)
    }
    
    it("should have size 4") {
      struct.size should equal (4L)
    }
  }
  
  describe("Padded Long Struct") {
    val struct = Struct.PaddedLong
    
    it("should have alignment 8") {
      struct.alignment should equal (8L)
    }
    
    it("should have size 8") {
      struct.size should equal (8L)
    }
  }
  
  describe("Padded Char Struct") {
    val struct = Struct.PaddedChar
    
    it("should have alignment 2") {
      struct.alignment should equal (2L)
    }
    
    it("should have size 2") {
      struct.size should equal (2L)
    }
  }
  
  describe("Padded Float Struct") {
    val struct = Struct.PaddedFloat
    
    it("should have alignment 4") {
      struct.alignment should equal (4L)
    }
    
    it("should have size 4") {
      struct.size should equal (4L)
    }
  }
  
  describe("Padded Double Struct") {
    val struct = Struct.PaddedDouble
    
    it("should have alignment 8") {
      struct.alignment should equal (8L)
    }
    
    it("should have size 8") {
      struct.size should equal (8L)
    }
  }
  
  describe("Packed Short Struct") {
    val struct = Struct.PackedShort
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 2") {
      struct.size should equal (2L)
    }
  }
  
  describe("Packed Int Struct") {
    val struct = Struct.PackedInt
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 4") {
      struct.size should equal (4L)
    }
  }
  
  describe("Packed Long Struct") {
    val struct = Struct.PackedLong
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 8") {
      struct.size should equal (8L)
    }
  }
  
  describe("Packed Char Struct") {
    val struct = Struct.PackedChar
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 2") {
      struct.size should equal (2L)
    }
  }
  
  describe("Packed Float Struct") {
    val struct = Struct.PackedFloat
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 4") {
      struct.size should equal (4L)
    }
  }
  
  describe("Packed Double Struct") {
    val struct = Struct.PackedDouble
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 8") {
      struct.size should equal (8L)
    }
  }
  
  describe("Packed Boolean Struct") {
    val struct = Struct.PackedBoolean
    
    it("should have alignment 1") {
      struct.alignment should equal (1L)
    }
    
    it("should have size 1") {
      struct.size should equal (1L)
    }
  }
}
