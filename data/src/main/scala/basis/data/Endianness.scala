//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

/** Significance ordering. */
sealed abstract class Endianness {
  /** Returns `true` if more significant elements precede less significant ones. */
  def isBig: Boolean

  /** Returns `true` if less significant elements precede more significant ones. */
  def isLittle: Boolean

  /** Returns `true` if this is the native ordering of the virtual machine. */
  def isNative: Boolean
}

/** Ordering where more significant elements precede less significant ones. */
sealed trait BigEndian extends Endianness

/** Ordering where less significant elememts precede more significant ones. */
sealed trait LittleEndian extends Endianness

/** Significance ordering of the host machine. */
sealed trait NativeEndian extends Endianness

private[data] final class BigEndianNative extends BigEndian with NativeEndian {
  final override def isBig: Boolean    = true
  final override def isLittle: Boolean = false
  final override def isNative: Boolean = true
  final override def toString: String  = "BigEndian"
}

private[data] final class BigEndianSwapped extends BigEndian {
  final override def isBig: Boolean    = true
  final override def isLittle: Boolean = false
  final override def isNative: Boolean = false
  final override def toString: String  = "BigEndian"
}

private[data] final class LittleEndianNative extends LittleEndian with NativeEndian {
  final override def isBig: Boolean    = false
  final override def isLittle: Boolean = true
  final override def isNative: Boolean = true
  final override def toString: String  = "LittleEndian"
}

private[data] final class LittleEndianSwapped extends LittleEndian with NativeEndian {
  final override def isBig: Boolean    = false
  final override def isLittle: Boolean = true
  final override def isNative: Boolean = false
  final override def toString: String  = "LittleEndian"
}
