//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

/** A significance ordering. */
sealed abstract class Endianness {
  /** Returns `true` if more significant elements precede less significant ones. */
  def isBig: Boolean

  /** Returns `true` if less significant elements precede more significant ones. */
  def isLittle: Boolean

  /** Returns `true` if this is the native ordering of the virtual machine. */
  def isNative: Boolean
}

/** An ordering where more significant elements precede less significant ones. */
final class BigEndian private[data] extends Endianness {
  final override def isBig: Boolean    = true
  final override def isLittle: Boolean = false
  final override val isNative: Boolean = java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN
  override def toString: String = "BigEndian"
}

/** An ordering where less significant elememts precede more significant ones. */
final class LittleEndian private[data] extends Endianness {
  final override def isBig: Boolean    = false
  final override def isLittle: Boolean = true
  final override val isNative: Boolean = java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN
  override def toString: String = "LittleEndian"
}
