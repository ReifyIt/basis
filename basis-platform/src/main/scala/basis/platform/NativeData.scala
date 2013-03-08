/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.platform

import basis.memory._

private[platform] abstract class NativeData(
    protected final val base: Long,
    final override val size: Long)
  extends Data {
  
  @native override def unit: Int
  
  override def endian: Endianness = NativeEndian
  
  override def isCoherent: Boolean = true
  
  @native override def loadByte(address: Long): Byte
  
  @native override def storeByte(address: Long, value: Byte): Unit
  
  @native override def loadShort(address: Long): Short
  
  @native override def storeShort(address: Long, value: Short): Unit
  
  @native override def loadInt(address: Long): Int
  
  @native override def storeInt(address: Long, value: Int): Unit
  
  @native override def loadLong(address: Long): Long
  
  @native override def storeLong(address: Long, value: Long): Unit
  
  @native override def loadFloat(address: Long): Float
  
  @native override def storeFloat(address: Long, value: Float): Unit
  
  @native override def loadDouble(address: Long): Double
  
  @native override def storeDouble(address: Long, value: Double): Unit
  
  @native override def loadUnalignedShort(address: Long): Short
  
  @native override def storeUnalignedShort(address: Long, value: Short): Unit
  
  @native override def loadUnalignedInt(address: Long): Int
  
  @native override def storeUnalignedInt(address: Long, value: Int): Unit
  
  @native override def loadUnalignedLong(address: Long): Long
  
  @native override def storeUnalignedLong(address: Long, value: Long): Unit
  
  @native override def loadUnalignedFloat(address: Long): Float
  
  @native override def storeUnalignedFloat(address: Long, value: Float): Unit
  
  @native override def loadUnalignedDouble(address: Long): Double
  
  @native override def storeUnalignedDouble(address: Long, value: Double): Unit
  
  @native override def loadVolatileByte(address: Long): Byte
  
  @native override def storeVolatileByte(address: Long, value: Byte): Unit
  
  @native override def loadVolatileShort(address: Long): Short
  
  @native override def storeVolatileShort(address: Long, value: Short): Unit
  
  @native override def loadVolatileInt(address: Long): Int
  
  @native override def storeVolatileInt(address: Long, value: Int): Unit
  
  @native override def loadVolatileLong(address: Long): Long
  
  @native override def storeVolatileLong(address: Long, value: Long): Unit
  
  @native override def loadVolatileFloat(address: Long): Float
  
  @native override def storeVolatileFloat(address: Long, value: Float): Unit
  
  @native override def loadVolatileDouble(address: Long): Double
  
  @native override def storeVolatileDouble(address: Long, value: Double): Unit
  
  @native override def compareAndSwapInt(address: Long, expected: Int, value: Int): Boolean
  
  @native override def compareAndSwapLong(address: Long, expected: Long, value: Long): Boolean
  
  @native override def compareAndSwapFloat(address: Long, expected: Float, value: Float): Boolean
  
  @native override def compareAndSwapDouble(address: Long, expected: Double, value: Double): Boolean
  
  @native override def move(fromAddress: Long, toAddress: Long, size: Long): Unit
  
  @native override def clear(fromAddress: Long, untilAddress: Long): Unit
}
