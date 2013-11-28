//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

trait ProxyLoader extends Loader {
  protected def underlying: Loader

  override def endian: Endianness = underlying.endian

  override def isCoherent: Boolean = underlying.isCoherent

  override def canLoad(offset: Long): Boolean =
    underlying.canLoad(offset)

  override def loadByte(offset: Long): Byte =
    underlying.loadByte(offset)

  override def loadShort(offset: Long): Short =
    underlying.loadShort(offset)

  override def loadInt(offset: Long): Int =
    underlying.loadInt(offset)

  override def loadLong(offset: Long): Long =
    underlying.loadLong(offset)

  override def loadFloat(offset: Long): Float =
    underlying.loadFloat(offset)

  override def loadDouble(offset: Long): Double =
    underlying.loadDouble(offset)

  override def loadUnalignedShort(offset: Long): Short =
    underlying.loadUnalignedShort(offset)

  override def loadUnalignedInt(offset: Long): Int =
    underlying.loadUnalignedInt(offset)

  override def loadUnalignedLong(offset: Long): Long =
    underlying.loadUnalignedLong(offset)

  override def loadUnalignedFloat(offset: Long): Float =
    underlying.loadUnalignedFloat(offset)

  override def loadUnalignedDouble(offset: Long): Double =
    underlying.loadUnalignedDouble(offset)

  override def loadVolatileByte(offset: Long): Byte =
    underlying.loadVolatileByte(offset)

  override def loadVolatileShort(offset: Long): Short =
    underlying.loadVolatileShort(offset)

  override def loadVolatileInt(offset: Long): Int =
    underlying.loadVolatileInt(offset)

  override def loadVolatileLong(offset: Long): Long =
    underlying.loadVolatileLong(offset)

  override def loadVolatileFloat(offset: Long): Float =
    underlying.loadVolatileFloat(offset)

  override def loadVolatileDouble(offset: Long): Double =
    underlying.loadVolatileDouble(offset)
}
