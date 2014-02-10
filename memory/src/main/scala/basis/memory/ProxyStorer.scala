//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

trait ProxyStorer extends Storer {
  protected def underlying: Storer

  override def endian: Endianness = underlying.endian

  override def isCoherent: Boolean = underlying.isCoherent

  override def canStore(offset: Long): Boolean =
    underlying.canStore(offset)

  override def storeByte(offset: Long, value: Byte): Unit =
    underlying.storeByte(offset, value)

  override def storeShort(offset: Long, value: Short): Unit =
    underlying.storeShort(offset, value)

  override def storeInt(offset: Long, value: Int): Unit =
    underlying.storeInt(offset, value)

  override def storeLong(offset: Long, value: Long): Unit =
    underlying.storeLong(offset, value)

  override def storeFloat(offset: Long, value: Float): Unit =
    underlying.storeFloat(offset, value)

  override def storeDouble(offset: Long, value: Double): Unit =
    underlying.storeDouble(offset, value)

  override def storeUnalignedShort(offset: Long, value: Short): Unit =
    underlying.storeUnalignedShort(offset, value)

  override def storeUnalignedInt(offset: Long, value: Int): Unit =
    underlying.storeUnalignedInt(offset, value)

  override def storeUnalignedLong(offset: Long, value: Long): Unit =
    underlying.storeUnalignedLong(offset, value)

  override def storeUnalignedFloat(offset: Long, value: Float): Unit =
    underlying.storeUnalignedFloat(offset, value)

  override def storeUnalignedDouble(offset: Long, value: Double): Unit =
    underlying.storeUnalignedDouble(offset, value)

  override def storeVolatileByte(offset: Long, value: Byte): Unit =
    underlying.storeVolatileByte(offset, value)

  override def storeVolatileShort(offset: Long, value: Short): Unit =
    underlying.storeVolatileShort(offset, value)

  override def storeVolatileInt(offset: Long, value: Int): Unit =
    underlying.storeVolatileInt(offset, value)

  override def storeVolatileLong(offset: Long, value: Long): Unit =
    underlying.storeVolatileLong(offset, value)

  override def storeVolatileFloat(offset: Long, value: Float): Unit =
    underlying.storeVolatileFloat(offset, value)

  override def storeVolatileDouble(offset: Long, value: Double): Unit =
    underlying.storeVolatileDouble(offset, value)
}
