/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala._

/** The ordering of bytes in multi-byte words.
  * 
  * @author Chris Sachs
  */
sealed abstract class ByteOrder private[data] {
  /** Returns `true` if this is the native byte order of the virtual machine. */
  def isNative: Boolean
}

/** The byte order where more significant bytes come first. */
object BigEndian extends ByteOrder {
  override val isNative: Boolean =
    java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN
  
  override def toString: java.lang.String = "BigEndian"
}

/** The byte order where less significant bytes come first. */
object LittleEndian extends ByteOrder {
  override val isNative: Boolean =
    java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN
  
  override def toString: java.lang.String = "LittleEndian"
}
