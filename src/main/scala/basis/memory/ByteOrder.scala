/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A significance ordering.
  * 
  * @author Chris Sachs
  */
sealed abstract class ByteOrder private[memory] {
  /** Returns `true` if this is the native `ByteOrder` of the virtual machine. */
  def isNative: Boolean
}

/** The ordering where more significant elements come first. */
object BigEndian extends ByteOrder {
  final override val isNative: Boolean =
    java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN
  
  override def toString: String = "BigEndian"
}

/** The ordering where less significant elements come first. */
object LittleEndian extends ByteOrder {
  final override val isNative: Boolean =
    java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN
  
  override def toString: String = "LittleEndian"
}
