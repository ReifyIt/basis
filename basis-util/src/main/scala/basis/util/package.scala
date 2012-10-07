/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object util {
  import scala.language.implicitConversions
  
  implicit def IntOps(value: Int): IntOps =
    throw new java.lang.UnsupportedOperationException
  
  implicit def RangeOps(range: Range): RangeOps =
    throw new java.lang.UnsupportedOperationException
}
