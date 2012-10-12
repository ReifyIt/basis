/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

private[basis] object ByteMacros {
  import scala.reflect.macros.Context
  
  def MinValue(c: Context): c.Expr[Byte] = c.literal(scala.Byte.MinValue)
  
  def MaxValue(c: Context): c.Expr[Byte] = c.literal(scala.Byte.MaxValue)
}
