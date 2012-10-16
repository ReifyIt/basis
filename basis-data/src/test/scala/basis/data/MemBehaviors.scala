/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait MemBehaviors extends ValTypeBehaviors { this: FunSpec =>
  import ShouldMatchers._
  import HexMatchers._
  
  def PrimitiveMemory(implicit allocator: Allocator) {
    import allocator.alloc
    
    it("should store Byte values") {
      val mem = alloc[Byte](1L)
      mem.storeByte(0L, 0xF7.toByte)
      mem.loadByte(0L) should equalByte (0xF7.toByte)
    }
    
    it("should store sequential Byte values") {
      val mem = alloc[Byte](3L)
      mem.storeByte(0L, 0xF7.toByte)
      mem.storeByte(1L, 0xE7.toByte)
      mem.storeByte(2L, 0xD7.toByte)
      withClue("1st value") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("2nd value") (mem.loadByte(1L) should equalByte (0xE7.toByte))
      withClue("3rd value") (mem.loadByte(2L) should equalByte (0xD7.toByte))
    }
    
    it("should store aligned Short values") {
      val mem = alloc[Short](1L)
      mem.storeShort(0L, 0xF7F6.toShort)
      mem.loadShort(0L) should equalShort (0xF7F6.toShort)
    }
    
    it("should store unaligned Short values") {
      val mem = alloc[Short](2L)
      mem.storeUnalignedShort(1L, 0xF7F6.toShort)
      mem.loadUnalignedShort(1L) should equalShort (0xF7F6.toShort)
    }
    
    it("should truncate unaligned addresses of aligned Short values") {
      val mem = alloc[Short](2L)
      mem.storeShort(1L, 0xF7F6.toShort)
      withClue("unaligned address") (mem.loadShort(1L) should equalShort (0xF7F6.toShort))
      withClue("truncated address") (mem.loadShort(0L) should equalShort (0xF7F6.toShort))
    }
    
    it("should store sequential Short values") {
      val mem = alloc[Short](3L)
      mem.storeShort(0L, 0xF7F6.toShort)
      mem.storeShort(2L, 0xE7E6.toShort)
      mem.storeShort(4L, 0xD7D6.toShort)
      withClue("1st value") (mem.loadShort(0L) should equalShort (0xF7F6.toShort))
      withClue("2nd value") (mem.loadShort(2L) should equalShort (0xE7E6.toShort))
      withClue("3rd value") (mem.loadShort(4L) should equalShort (0xD7D6.toShort))
    }
    
    it("should store aligned Int values") {
      val mem = alloc[Int](1L)
      mem.storeInt(0L, 0xF7F6F5F4)
      mem.loadInt(0L) should equalInt (0xF7F6F5F4)
    }
    
    it("should store unaligned Int values") {
      val mem = alloc[Int](2L)
      mem.storeUnalignedInt(3L, 0xF7F6F5F4)
      mem.loadUnalignedInt(3L) should equalInt (0xF7F6F5F4)
    }
    
    it("should truncate unaligned addresses of aligned Int values") {
      val mem = alloc[Int](2L)
      mem.storeInt(3L, 0xF7F6F5F4)
      withClue("unaligned address") (mem.loadInt(3L) should equalInt (0xF7F6F5F4))
      withClue("truncated address") (mem.loadInt(0L) should equalInt (0xF7F6F5F4))
    }
    
    it("should store sequential Int values") {
      val mem = alloc[Int](3L)
      mem.storeInt(0L, 0xF7F6F5F4)
      mem.storeInt(4L, 0xE7E6E5E4)
      mem.storeInt(8L, 0xD7D6D5D4)
      withClue("1st value") (mem.loadInt(0L) should equalInt (0xF7F6F5F4))
      withClue("2nd value") (mem.loadInt(4L) should equalInt (0xE7E6E5E4))
      withClue("3rd value") (mem.loadInt(8L) should equalInt (0xD7D6D5D4))
    }
    
    it("should store aligned Long values") {
      val mem = alloc[Long](1L)
      mem.storeLong(0L, 0xF7F6F5F4F3F2F1F0L)
      mem.loadLong(0L) should equalLong (0xF7F6F5F4F3F2F1F0L)
    }
    
    it("should store unaligned Long values") {
      val mem = alloc[Long](2L)
      mem.storeUnalignedLong(7L, 0xF7F6F5F4F3F2F1F0L)
      mem.loadUnalignedLong(7L) should equalLong (0xF7F6F5F4F3F2F1F0L)
    }
    
    it("should truncate unaligned addresses of aligned Long values") {
      val mem = alloc[Long](2L)
      mem.storeLong(7L, 0xF7F6F5F4F3F2F1F0L)
      withClue("unaligned address") (mem.loadLong(7L) should equalLong (0xF7F6F5F4F3F2F1F0L))
      withClue("truncated address") (mem.loadLong(0L) should equalLong (0xF7F6F5F4F3F2F1F0L))
    }
    
    it("should store sequential Long values") {
      val mem = alloc[Long](3L)
      mem.storeLong(0L,  0xF7F6F5F4F3F2F1F0L)
      mem.storeLong(8L,  0xE7E6E5E4E3E2E1E0L)
      mem.storeLong(16L, 0xD7D6D5D4D3D2D1D0L)
      withClue("1st value") (mem.loadLong(0L)  should equalLong (0xF7F6F5F4F3F2F1F0L))
      withClue("2nd value") (mem.loadLong(8L)  should equalLong (0xE7E6E5E4E3E2E1E0L))
      withClue("3rd value") (mem.loadLong(16L) should equalLong (0xD7D6D5D4D3D2D1D0L))
    }
    
    it("should store aligned Float values") {
      val mem = alloc[Float](1L)
      mem.storeFloat(0L, intBitsToFloat(0xF7F6F5F4))
      mem.loadFloat(0L) should equalFloat (intBitsToFloat(0xF7F6F5F4))
    }
    
    it("should store unaligned Float values") {
      val mem = alloc[Float](2L)
      mem.storeUnalignedFloat(3L, intBitsToFloat(0xF7F6F5F4))
      mem.loadUnalignedFloat(3L) should equalFloat (intBitsToFloat(0xF7F6F5F4))
    }
    
    it("should truncate unaligned addresses of aligned Float values") {
      val mem = alloc[Float](2L)
      mem.storeFloat(3L, intBitsToFloat(0xF7F6F5F4))
      withClue("unaligned address") (mem.loadFloat(3L) should equalFloat (intBitsToFloat(0xF7F6F5F4)))
      withClue("truncated address") (mem.loadFloat(0L) should equalFloat (intBitsToFloat(0xF7F6F5F4)))
    }
    
    it("should store sequential Float values") {
      val mem = alloc[Float](3L)
      mem.storeFloat(0L, intBitsToFloat(0xF7F6F5F4))
      mem.storeFloat(4L, intBitsToFloat(0xE7E6E5E4))
      mem.storeFloat(8L, intBitsToFloat(0xD7D6D5D4))
      withClue("1st value") (mem.loadFloat(0L) should equalFloat (intBitsToFloat(0xF7F6F5F4)))
      withClue("2nd value") (mem.loadFloat(4L) should equalFloat (intBitsToFloat(0xE7E6E5E4)))
      withClue("3rd value") (mem.loadFloat(8L) should equalFloat (intBitsToFloat(0xD7D6D5D4)))
    }
    
    it("should store aligned Double values") {
      val mem = alloc[Double](1L)
      mem.storeDouble(0L, longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      mem.loadDouble(0L) should equalDouble(longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
    }
    
    it("should store unaligned Double values") {
      val mem = alloc[Double](2L)
      mem.storeUnalignedDouble(7L, longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      mem.loadUnalignedDouble(7L) should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
    }
    
    it("should truncate unaligned addresses of aligned Double values") {
      val mem = alloc[Double](2L)
      mem.storeDouble(7L, longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("unaligned address") (mem.loadDouble(7L) should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L)))
      withClue("truncated address") (mem.loadDouble(0L) should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L)))
    }
    
    it("should store sequential Double values") {
      val mem = alloc[Double](3L)
      mem.storeDouble(0L,  longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      mem.storeDouble(8L,  longBitsToDouble(0xE7E6E5E4E3E2E1E0L))
      mem.storeDouble(16L, longBitsToDouble(0xD7D6D5D4D3D2D1D0L))
      withClue("1st value") (mem.loadDouble(0L)  should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L)))
      withClue("2nd value") (mem.loadDouble(8L)  should equalDouble (longBitsToDouble(0xE7E6E5E4E3E2E1E0L)))
      withClue("3rd value") (mem.loadDouble(16L) should equalDouble (longBitsToDouble(0xD7D6D5D4D3D2D1D0L)))
    }
  }
  
  def BigEndianMemory(implicit allocator: Allocator) {
    import allocator.alloc
    
    it("should be big-endian") {
      val mem = alloc[Byte](0L)
      mem.endian should be (BigEndian)
    }
    
    it("should store big-endian aligned Short values") {
      val mem = alloc[Short](1L)
      mem.storeShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
    }
    
    it("should store big-endian unaligned Short values") {
      val mem = alloc[Short](1L)
      mem.storeUnalignedShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
    }
    
    it("should store big-endian aligned Int values") {
      val mem = alloc[Int](1L)
      mem.storeInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian unaligned Int values") {
      val mem = alloc[Int](1L)
      mem.storeUnalignedInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian aligned Long values") {
      val mem = alloc[Long](1L)
      mem.storeLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF0.toByte))
    }
    
    it("should store big-endian unaligned Long values") {
      val mem = alloc[Long](1L)
      mem.storeUnalignedLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF0.toByte))
    }
    
    it("should store big-endian aligned Float values") {
      val mem = alloc[Float](1L)
      mem.storeFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian unaligned Float values") {
      val mem = alloc[Float](1L)
      mem.storeUnalignedFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian aligned Double values") {
      val mem = alloc[Double](1L)
      mem.storeDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF0.toByte))
    }
    
    it("should store big-endian unaligned Double values") {
      val mem = alloc[Double](1L)
      mem.storeUnalignedDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF0.toByte))
    }
  }
  
  def LittleEndianMemory(implicit allocator: Allocator) {
    import allocator.alloc
    
    it("should be little-endian") {
      val mem = alloc[Byte](0L)
      mem.endian should be (LittleEndian)
    }
    
    it("should store little-endian aligned Short values") {
      val mem = alloc[Short](1L)
      mem.storeShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF6.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Short values") {
      val mem = alloc[Short](1L)
      mem.storeUnalignedShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF6.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Int values") {
      val mem = alloc[Int](1L)
      mem.storeInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Int values") {
      val mem = alloc[Int](1L)
      mem.storeUnalignedInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Long values") {
      val mem = alloc[Long](1L)
      mem.storeLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Long values") {
      val mem = alloc[Long](1L)
      mem.storeUnalignedLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Float values") {
      val mem = alloc[Float](1L)
      mem.storeFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Float values") {
      val mem = alloc[Float](1L)
      mem.storeUnalignedFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Double values") {
      val mem = alloc[Double](1L)
      mem.storeDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Double values") {
      val mem = alloc[Double](1L)
      mem.storeUnalignedDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (mem.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (mem.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (mem.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (mem.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (mem.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (mem.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (mem.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (mem.loadByte(7L) should equalByte (0xF7.toByte))
    }
  }
  
  def NativeEndianMemory(implicit allocator: Allocator) = NativeEndian match {
    case BigEndian => BigEndianMemory(allocator)
    case LittleEndian => LittleEndianMemory(allocator)
  }
  
  def StructuredMemory(implicit allocator: Allocator) {
    import ValType._
    
    describe("Packed Byte memory") {
      val struct = PackedByte
      it should behave like ValueType(0x7F.toByte)(allocator, struct)
    }
    
    describe("Packed Short memory") {
      val struct = PackedShort
      it should behave like ValueType(0x7F6F.toShort)(allocator, struct)
    }
    
    describe("Packed Int memory") {
      val struct = PackedInt
      it should behave like ValueType(0x7F6F5F4F)(allocator, struct)
    }
    
    describe("Packed Long memory") {
      val struct = PackedLong
      it should behave like ValueType(0x7F6F5F4F3F2F1F0FL)(allocator, struct)
    }
    
    describe("Packed Float memory") {
      val struct = PackedFloat
      it should behave like ValueType(intBitsToFloat(0x7F6F5F4F))(allocator, struct)
    }
    
    describe("Packed Double memory") {
      val struct = PackedDouble
      it should behave like ValueType(longBitsToDouble(0x7F6F5F4F3F2F1F0FL))(allocator, struct)
    }
    
    describe("Packed Boolean memory") {
      val struct = PackedBoolean
      it should behave like ValueType(true)(allocator, struct)
    }
    
    describe("Padded Short memory") {
      val struct = PaddedShort
      it should behave like ValueType(0x7F6F.toShort)(allocator, struct)
    }
    
    describe("Padded Int memory") {
      val struct = PaddedInt
      it should behave like ValueType(0x7F6F5F4F)(allocator, struct)
    }
    
    describe("Padded Long memory") {
      val struct = PaddedLong
      it should behave like ValueType(0x7F6F5F4F3F2F1F0FL)(allocator, struct)
    }
    
    describe("Padded Float memory") {
      val struct = PaddedFloat
      it should behave like ValueType(intBitsToFloat(0x7F6F5F4F))(allocator, struct)
    }
    
    describe("Padded Double memory") {
      val struct = PaddedDouble
      it should behave like ValueType(longBitsToDouble(0x7F6F5F4F3F2F1F0FL))(allocator, struct)
    }
  }
}
