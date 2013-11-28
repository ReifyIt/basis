//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

abstract class ProxyPointer extends Pointer with ProxyLoader with ProxyStorer with ProxyReader with ProxyWriter {
  protected override def underlying: Pointer

  override def endian: Endianness = underlying.endian

  override def isCoherent: Boolean = underlying.isCoherent

  override def += (offset: Long): Unit =
    underlying += offset

  override def -= (offset: Long): Unit =
    underlying -= offset

  override def align(alignment: Long): Unit =
    underlying.align(alignment)

  override def truncate(alignment: Long): Unit =
    underlying.truncate(alignment)
}
