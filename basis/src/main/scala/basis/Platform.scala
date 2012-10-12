/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

abstract class Platform extends Primitives {
  import scala.language.experimental.macros
  
  final def equal[T](x: T, y: T)(implicit T: Equal[T]): Boolean =
    macro PlatformMacros.equal[T]
  
  final def hash[T](x: T)(implicit T: Hash[T]): Int =
    macro PlatformMacros.hash[T]
  
  final def show[T](x: T)(implicit T: Show[T], buffer: CharBuffer): buffer.State =
    macro PlatformMacros.show[T]
}
