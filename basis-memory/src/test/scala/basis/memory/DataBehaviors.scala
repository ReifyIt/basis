/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait DataBehaviors extends ValTypeBehaviors { this: FunSpec =>
  import ShouldMatchers._
  import HexMatchers._
  
  def PrimitiveData(implicit allocator: Allocator) {
    import allocator.alloc
    
    it("should store Byte values") {
      val data = alloc[Byte](1L)
      data.storeByte(0L, 0xF7.toByte)
      data.loadByte(0L) should equalByte (0xF7.toByte)
    }
    
    it("should store sequential Byte values") {
      val data = alloc[Byte](3L)
      data.storeByte(0L, 0xF7.toByte)
      data.storeByte(1L, 0xE7.toByte)
      data.storeByte(2L, 0xD7.toByte)
      withClue("1st value") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("2nd value") (data.loadByte(1L) should equalByte (0xE7.toByte))
      withClue("3rd value") (data.loadByte(2L) should equalByte (0xD7.toByte))
    }
    
    it("should store aligned Short values") {
      val data = alloc[Short](1L)
      data.storeShort(0L, 0xF7F6.toShort)
      data.loadShort(0L) should equalShort (0xF7F6.toShort)
    }
    
    it("should store unaligned Short values") {
      val data = alloc[Short](2L)
      data.storeUnalignedShort(1L, 0xF7F6.toShort)
      data.loadUnalignedShort(1L) should equalShort (0xF7F6.toShort)
    }
    
    it("should truncate unaligned addresses of aligned Short values") {
      val data = alloc[Short](2L)
      data.storeShort(1L, 0xF7F6.toShort)
      withClue("unaligned address") (data.loadShort(1L) should equalShort (0xF7F6.toShort))
      withClue("truncated address") (data.loadShort(0L) should equalShort (0xF7F6.toShort))
    }
    
    it("should store sequential Short values") {
      val data = alloc[Short](3L)
      data.storeShort(0L, 0xF7F6.toShort)
      data.storeShort(2L, 0xE7E6.toShort)
      data.storeShort(4L, 0xD7D6.toShort)
      withClue("1st value") (data.loadShort(0L) should equalShort (0xF7F6.toShort))
      withClue("2nd value") (data.loadShort(2L) should equalShort (0xE7E6.toShort))
      withClue("3rd value") (data.loadShort(4L) should equalShort (0xD7D6.toShort))
    }
    
    it("should store aligned Int values") {
      val data = alloc[Int](1L)
      data.storeInt(0L, 0xF7F6F5F4)
      data.loadInt(0L) should equalInt (0xF7F6F5F4)
    }
    
    it("should store unaligned Int values") {
      val data = alloc[Int](2L)
      data.storeUnalignedInt(3L, 0xF7F6F5F4)
      data.loadUnalignedInt(3L) should equalInt (0xF7F6F5F4)
    }
    
    it("should truncate unaligned addresses of aligned Int values") {
      val data = alloc[Int](2L)
      data.storeInt(3L, 0xF7F6F5F4)
      withClue("unaligned address") (data.loadInt(3L) should equalInt (0xF7F6F5F4))
      withClue("truncated address") (data.loadInt(0L) should equalInt (0xF7F6F5F4))
    }
    
    it("should store sequential Int values") {
      val data = alloc[Int](3L)
      data.storeInt(0L, 0xF7F6F5F4)
      data.storeInt(4L, 0xE7E6E5E4)
      data.storeInt(8L, 0xD7D6D5D4)
      withClue("1st value") (data.loadInt(0L) should equalInt (0xF7F6F5F4))
      withClue("2nd value") (data.loadInt(4L) should equalInt (0xE7E6E5E4))
      withClue("3rd value") (data.loadInt(8L) should equalInt (0xD7D6D5D4))
    }
    
    it("should store aligned Long values") {
      val data = alloc[Long](1L)
      data.storeLong(0L, 0xF7F6F5F4F3F2F1F0L)
      data.loadLong(0L) should equalLong (0xF7F6F5F4F3F2F1F0L)
    }
    
    it("should store unaligned Long values") {
      val data = alloc[Long](2L)
      data.storeUnalignedLong(7L, 0xF7F6F5F4F3F2F1F0L)
      data.loadUnalignedLong(7L) should equalLong (0xF7F6F5F4F3F2F1F0L)
    }
    
    it("should truncate unaligned addresses of aligned Long values") {
      val data = alloc[Long](2L)
      data.storeLong(7L, 0xF7F6F5F4F3F2F1F0L)
      withClue("unaligned address") (data.loadLong(7L) should equalLong (0xF7F6F5F4F3F2F1F0L))
      withClue("truncated address") (data.loadLong(0L) should equalLong (0xF7F6F5F4F3F2F1F0L))
    }
    
    it("should store sequential Long values") {
      val data = alloc[Long](3L)
      data.storeLong(0L,  0xF7F6F5F4F3F2F1F0L)
      data.storeLong(8L,  0xE7E6E5E4E3E2E1E0L)
      data.storeLong(16L, 0xD7D6D5D4D3D2D1D0L)
      withClue("1st value") (data.loadLong(0L)  should equalLong (0xF7F6F5F4F3F2F1F0L))
      withClue("2nd value") (data.loadLong(8L)  should equalLong (0xE7E6E5E4E3E2E1E0L))
      withClue("3rd value") (data.loadLong(16L) should equalLong (0xD7D6D5D4D3D2D1D0L))
    }
    
    it("should store aligned Float values") {
      val data = alloc[Float](1L)
      data.storeFloat(0L, intBitsToFloat(0xF7F6F5F4))
      data.loadFloat(0L) should equalFloat (intBitsToFloat(0xF7F6F5F4))
    }
    
    it("should store unaligned Float values") {
      val data = alloc[Float](2L)
      data.storeUnalignedFloat(3L, intBitsToFloat(0xF7F6F5F4))
      data.loadUnalignedFloat(3L) should equalFloat (intBitsToFloat(0xF7F6F5F4))
    }
    
    it("should truncate unaligned addresses of aligned Float values") {
      val data = alloc[Float](2L)
      data.storeFloat(3L, intBitsToFloat(0xF7F6F5F4))
      withClue("unaligned address") (data.loadFloat(3L) should equalFloat (intBitsToFloat(0xF7F6F5F4)))
      withClue("truncated address") (data.loadFloat(0L) should equalFloat (intBitsToFloat(0xF7F6F5F4)))
    }
    
    it("should store sequential Float values") {
      val data = alloc[Float](3L)
      data.storeFloat(0L, intBitsToFloat(0xF7F6F5F4))
      data.storeFloat(4L, intBitsToFloat(0xE7E6E5E4))
      data.storeFloat(8L, intBitsToFloat(0xD7D6D5D4))
      withClue("1st value") (data.loadFloat(0L) should equalFloat (intBitsToFloat(0xF7F6F5F4)))
      withClue("2nd value") (data.loadFloat(4L) should equalFloat (intBitsToFloat(0xE7E6E5E4)))
      withClue("3rd value") (data.loadFloat(8L) should equalFloat (intBitsToFloat(0xD7D6D5D4)))
    }
    
    it("should store aligned Double values") {
      val data = alloc[Double](1L)
      data.storeDouble(0L, longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      data.loadDouble(0L) should equalDouble(longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
    }
    
    it("should store unaligned Double values") {
      val data = alloc[Double](2L)
      data.storeUnalignedDouble(7L, longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      data.loadUnalignedDouble(7L) should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
    }
    
    it("should truncate unaligned addresses of aligned Double values") {
      val data = alloc[Double](2L)
      data.storeDouble(7L, longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("unaligned address") (data.loadDouble(7L) should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L)))
      withClue("truncated address") (data.loadDouble(0L) should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L)))
    }
    
    it("should store sequential Double values") {
      val data = alloc[Double](3L)
      data.storeDouble(0L,  longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      data.storeDouble(8L,  longBitsToDouble(0xE7E6E5E4E3E2E1E0L))
      data.storeDouble(16L, longBitsToDouble(0xD7D6D5D4D3D2D1D0L))
      withClue("1st value") (data.loadDouble(0L)  should equalDouble (longBitsToDouble(0xF7F6F5F4F3F2F1F0L)))
      withClue("2nd value") (data.loadDouble(8L)  should equalDouble (longBitsToDouble(0xE7E6E5E4E3E2E1E0L)))
      withClue("3rd value") (data.loadDouble(16L) should equalDouble (longBitsToDouble(0xD7D6D5D4D3D2D1D0L)))
    }
  }
  
  def BigEndianData(implicit allocator: Allocator) {
    import allocator.alloc
    
    it("should be big-endian") {
      val data = alloc[Byte](0L)
      data.endian should be (BigEndian)
    }
    
    it("should store big-endian aligned Short values") {
      val data = alloc[Short](1L)
      data.storeShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
    }
    
    it("should store big-endian unaligned Short values") {
      val data = alloc[Short](1L)
      data.storeUnalignedShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
    }
    
    it("should store big-endian aligned Int values") {
      val data = alloc[Int](1L)
      data.storeInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian unaligned Int values") {
      val data = alloc[Int](1L)
      data.storeUnalignedInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian aligned Long values") {
      val data = alloc[Long](1L)
      data.storeLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF0.toByte))
    }
    
    it("should store big-endian unaligned Long values") {
      val data = alloc[Long](1L)
      data.storeUnalignedLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF0.toByte))
    }
    
    it("should store big-endian aligned Float values") {
      val data = alloc[Float](1L)
      data.storeFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian unaligned Float values") {
      val data = alloc[Float](1L)
      data.storeUnalignedFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
    }
    
    it("should store big-endian aligned Double values") {
      val data = alloc[Double](1L)
      data.storeDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF0.toByte))
    }
    
    it("should store big-endian unaligned Double values") {
      val data = alloc[Double](1L)
      data.storeUnalignedDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF7.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF6.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF5.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF4.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF3.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF2.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF1.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF0.toByte))
    }
  }
  
  def LittleEndianData(implicit allocator: Allocator) {
    import allocator.alloc
    
    it("should be little-endian") {
      val data = alloc[Byte](0L)
      data.endian should be (LittleEndian)
    }
    
    it("should store little-endian aligned Short values") {
      val data = alloc[Short](1L)
      data.storeShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF6.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Short values") {
      val data = alloc[Short](1L)
      data.storeUnalignedShort(0L, 0xF7F6.toShort)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF6.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Int values") {
      val data = alloc[Int](1L)
      data.storeInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Int values") {
      val data = alloc[Int](1L)
      data.storeUnalignedInt(0L, 0xF7F6F5F4)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Long values") {
      val data = alloc[Long](1L)
      data.storeLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Long values") {
      val data = alloc[Long](1L)
      data.storeUnalignedLong(0L, 0xF7F6F5F4F3F2F1F0L)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Float values") {
      val data = alloc[Float](1L)
      data.storeFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Float values") {
      val data = alloc[Float](1L)
      data.storeUnalignedFloat(0L, java.lang.Float.intBitsToFloat(0xF7F6F5F4))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF4.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF5.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF6.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian aligned Double values") {
      val data = alloc[Double](1L)
      data.storeDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF7.toByte))
    }
    
    it("should store little-endian unaligned Double values") {
      val data = alloc[Double](1L)
      data.storeUnalignedDouble(0L, java.lang.Double.longBitsToDouble(0xF7F6F5F4F3F2F1F0L))
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0xF1.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xF2.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xF3.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xF4.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xF5.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0xF6.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF7.toByte))
    }
  }
  
  def NativeEndianData(implicit allocator: Allocator) = NativeEndian match {
    case BigEndian => BigEndianData(allocator)
    case LittleEndian => LittleEndianData(allocator)
  }
  
  def StructuredData(implicit allocator: Allocator) {
    import ValType._
    
    describe("Packed Byte data") {
      val struct = PackedByte
      it should behave like ValueType(0x7F.toByte)(allocator, struct)
    }
    
    describe("Packed Short data") {
      val struct = PackedShort
      it should behave like ValueType(0x7F6F.toShort)(allocator, struct)
    }
    
    describe("Packed Int data") {
      val struct = PackedInt
      it should behave like ValueType(0x7F6F5F4F)(allocator, struct)
    }
    
    describe("Packed Long data") {
      val struct = PackedLong
      it should behave like ValueType(0x7F6F5F4F3F2F1F0FL)(allocator, struct)
    }
    
    describe("Packed Float data") {
      val struct = PackedFloat
      it should behave like ValueType(intBitsToFloat(0x7F6F5F4F))(allocator, struct)
    }
    
    describe("Packed Double data") {
      val struct = PackedDouble
      it should behave like ValueType(longBitsToDouble(0x7F6F5F4F3F2F1F0FL))(allocator, struct)
    }
    
    describe("Packed Boolean data") {
      val struct = PackedBoolean
      it should behave like ValueType(true)(allocator, struct)
    }
    
    describe("Padded Short data") {
      val struct = PaddedShort
      it should behave like ValueType(0x7F6F.toShort)(allocator, struct)
    }
    
    describe("Padded Int data") {
      val struct = PaddedInt
      it should behave like ValueType(0x7F6F5F4F)(allocator, struct)
    }
    
    describe("Padded Long data") {
      val struct = PaddedLong
      it should behave like ValueType(0x7F6F5F4F3F2F1F0FL)(allocator, struct)
    }
    
    describe("Padded Float data") {
      val struct = PaddedFloat
      it should behave like ValueType(intBitsToFloat(0x7F6F5F4F))(allocator, struct)
    }
    
    describe("Padded Double data") {
      val struct = PaddedDouble
      it should behave like ValueType(longBitsToDouble(0x7F6F5F4F3F2F1F0FL))(allocator, struct)
    }
  }
}
