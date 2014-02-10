//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

trait ProxyReader extends ProxyLoader with Reader {
  protected override def underlying: Reader

  override def += (offset: Long): Unit =
    underlying += offset

  override def -= (offset: Long): Unit =
    underlying -= offset

  override def align(alignment: Long): Unit =
    underlying.align(alignment)

  override def truncate(alignment: Long): Unit =
    underlying.truncate(alignment)

  override def readByte(): Byte =
    underlying.readByte()

  override def readShort(): Short =
    underlying.readShort()

  override def readInt(): Int =
    underlying.readInt()

  override def readLong(): Long =
    underlying.readLong()

  override def readFloat(): Float =
    underlying.readFloat()

  override def readDouble(): Double =
    underlying.readDouble()

  override def readUnalignedShort(): Short =
    underlying.readUnalignedShort()

  override def readUnalignedInt(): Int =
    underlying.readUnalignedInt()

  override def readUnalignedLong(): Long =
    underlying.readUnalignedLong()

  override def readUnalignedFloat(): Float =
    underlying.readUnalignedFloat()

  override def readUnalignedDouble(): Double =
    underlying.readUnalignedDouble()
}
