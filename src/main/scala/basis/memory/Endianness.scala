/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import java.nio.ByteOrder

/** Represents a significance ordering.
  * 
  * @author Christopher Sachs
  */
sealed abstract class Endianness {
  /** Returns `true` if this is the Endianness of the virtual machine. */
  def isNative: Boolean
}

/** Contains an enumeration of significance orderings. Implicitly provides the
  * `NativeEndian` ordering. */
object Endianness {
  /** The ordering of the virtual machine. */
  implicit val NativeEndian: Endianness = ByteOrder.nativeOrder match {
    case ByteOrder.BIG_ENDIAN => BigEndian
    case ByteOrder.LITTLE_ENDIAN => LittleEndian
  }
  
  /** The ordering where more significant elements come first. */
  object BigEndian extends Endianness {
    final val isNative: Boolean = ByteOrder.nativeOrder eq ByteOrder.BIG_ENDIAN
    
    override def toString: String = "BigEndian"
  }
  
  /** The ordering where less significant elements come first. */
  object LittleEndian extends Endianness {
    final val isNative: Boolean = ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN
    
    override def toString: String = "LittleEndian"
  }
}
