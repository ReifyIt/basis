/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

/** A significance ordering.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
sealed abstract class Endianness {
  /** Returns `true` if this is the native ordering of the virtual machine. */
  def isNative: Boolean
}

/** An ordering where more significant elements come before less significant ones. */
object BigEndian extends Endianness {
  final override val isNative: Boolean = java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN
  override def toString: String = "BigEndian"
}

/** An ordering where less significant elememts come before more significant ones. */
object LittleEndian extends Endianness {
  final override val isNative: Boolean = java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN
  override def toString: String = "LittleEndian"
}
