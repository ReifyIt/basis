//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

trait ProxyWriter extends ProxyStorer with Writer {
  protected override def underlying: Writer

  override def += (offset: Long): Unit =
    underlying += offset

  override def -= (offset: Long): Unit =
    underlying -= offset

  override def align(alignment: Long): Unit =
    underlying.align(alignment)

  override def truncate(alignment: Long): Unit =
    underlying.truncate(alignment)

  override def writeByte(value: Byte): Unit =
    underlying.writeByte(value)

  override def writeShort(value: Short): Unit =
    underlying.writeShort(value)

  override def writeInt(value: Int): Unit =
    underlying.writeInt(value)

  override def writeLong(value: Long): Unit =
    underlying.writeLong(value)

  override def writeFloat(value: Float): Unit =
    underlying.writeFloat(value)

  override def writeDouble(value: Double): Unit =
    underlying.writeDouble(value)

  override def writeUnalignedShort(value: Short): Unit =
    underlying.writeUnalignedShort(value)

  override def writeUnalignedInt(value: Int): Unit =
    underlying.writeUnalignedInt(value)

  override def writeUnalignedLong(value: Long): Unit =
    underlying.writeUnalignedLong(value)

  override def writeUnalignedFloat(value: Float): Unit =
    underlying.writeUnalignedFloat(value)

  override def writeUnalignedDouble(value: Double): Unit =
    underlying.writeUnalignedDouble(value)
}
