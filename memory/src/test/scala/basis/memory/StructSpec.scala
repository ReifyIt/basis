//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import org.scalatest._

class StructSpec extends FunSpec {
  import Matchers._

  override def suiteName = "Struct specification"

  describe("Packed Byte") {
    val struct = Struct.PackedByte

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 1") {
      struct.size should equal (1)
    }
  }

  describe("Packed Short") {
    val struct = Struct.PackedShort

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 2") {
      struct.size should equal (2)
    }
  }

  describe("Packed Int") {
    val struct = Struct.PackedInt

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 4") {
      struct.size should equal (4)
    }
  }

  describe("Packed Long") {
    val struct = Struct.PackedLong

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 8") {
      struct.size should equal (8)
    }
  }

  describe("Packed Float") {
    val struct = Struct.PackedFloat

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 4") {
      struct.size should equal (4)
    }
  }

  describe("Packed Double") {
    val struct = Struct.PackedDouble

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 8") {
      struct.size should equal (8)
    }
  }

  describe("Packed Boolean") {
    val struct = Struct.PackedBoolean

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 1") {
      struct.size should equal (1)
    }
  }

  describe("Padded Short") {
    val struct = Struct.PaddedShort

    it("should have alignment 2") {
      struct.alignment should equal (2)
    }

    it("should have size 2") {
      struct.size should equal (2)
    }
  }

  describe("Padded Int") {
    val struct = Struct.PaddedInt

    it("should have alignment 4") {
      struct.alignment should equal (4)
    }

    it("should have size 4") {
      struct.size should equal (4)
    }
  }

  describe("Padded Long") {
    val struct = Struct.PaddedLong

    it("should have alignment 8") {
      struct.alignment should equal (8)
    }

    it("should have size 8") {
      struct.size should equal (8)
    }
  }

  describe("Padded Float") {
    val struct = Struct.PaddedFloat

    it("should have alignment 4") {
      struct.alignment should equal (4)
    }

    it("should have size 4") {
      struct.size should equal (4)
    }
  }

  describe("Padded Double") {
    val struct = Struct.PaddedDouble

    it("should have alignment 8") {
      struct.alignment should equal (8)
    }

    it("should have size 8") {
      struct.size should equal (8)
    }
  }

  describe("Volatile Byte") {
    val struct = Struct.VolatileByte

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 1") {
      struct.size should equal (1)
    }
  }

  describe("Volatile Short") {
    val struct = Struct.VolatileShort

    it("should have alignment 2") {
      struct.alignment should equal (2)
    }

    it("should have size 2") {
      struct.size should equal (2)
    }
  }

  describe("Volatile Int") {
    val struct = Struct.VolatileInt

    it("should have alignment 4") {
      struct.alignment should equal (4)
    }

    it("should have size 4") {
      struct.size should equal (4)
    }
  }

  describe("Volatile Long") {
    val struct = Struct.VolatileLong

    it("should have alignment 8") {
      struct.alignment should equal (8)
    }

    it("should have size 8") {
      struct.size should equal (8)
    }
  }

  describe("Volatile Float") {
    val struct = Struct.VolatileFloat

    it("should have alignment 4") {
      struct.alignment should equal (4)
    }

    it("should have size 4") {
      struct.size should equal (4)
    }
  }

  describe("Volatile Double") {
    val struct = Struct.VolatileDouble

    it("should have alignment 8") {
      struct.alignment should equal (8)
    }

    it("should have size 8") {
      struct.size should equal (8)
    }
  }

  describe("Volatile Boolean") {
    val struct = Struct.VolatileBoolean

    it("should have alignment 1") {
      struct.alignment should equal (1)
    }

    it("should have size 1") {
      struct.size should equal (1)
    }
  }
}
