/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class StructSpec extends FunSpec with ShouldMatchers {
  import Struct._
  
  override def suiteName = "Struct specification"
  
  describe("Packed Byte") {
    val struct = PackedByte
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 1") {
      struct.size should equal (1)
    }
  }
  
  describe("Packed Short") {
    val struct = PackedShort
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 2") {
      struct.size should equal (2)
    }
  }
  
  describe("Packed Int") {
    val struct = PackedInt
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 4") {
      struct.size should equal (4)
    }
  }
  
  describe("Packed Long") {
    val struct = PackedLong
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 8") {
      struct.size should equal (8)
    }
  }
  
  describe("Packed Float") {
    val struct = PackedFloat
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 4") {
      struct.size should equal (4)
    }
  }
  
  describe("Packed Double") {
    val struct = PackedDouble
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 8") {
      struct.size should equal (8)
    }
  }
  
  describe("Packed Boolean") {
    val struct = PackedBoolean
    
    it("should have alignment 1") {
      struct.alignment should equal (1)
    }
    
    it("should have size 1") {
      struct.size should equal (1)
    }
  }
  
  describe("Padded Short") {
    val struct = PaddedShort
    
    it("should have alignment 2") {
      struct.alignment should equal (2)
    }
    
    it("should have size 2") {
      struct.size should equal (2)
    }
  }
  
  describe("Padded Int") {
    val struct = PaddedInt
    
    it("should have alignment 4") {
      struct.alignment should equal (4)
    }
    
    it("should have size 4") {
      struct.size should equal (4)
    }
  }
  
  describe("Padded Long") {
    val struct = PaddedLong
    
    it("should have alignment 8") {
      struct.alignment should equal (8)
    }
    
    it("should have size 8") {
      struct.size should equal (8)
    }
  }
  
  describe("Padded Float") {
    val struct = PaddedFloat
    
    it("should have alignment 4") {
      struct.alignment should equal (4)
    }
    
    it("should have size 4") {
      struct.size should equal (4)
    }
  }
  
  describe("Padded Double") {
    val struct = PaddedDouble
    
    it("should have alignment 8") {
      struct.alignment should equal (8)
    }
    
    it("should have size 8") {
      struct.size should equal (8)
    }
  }
}
