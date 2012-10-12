/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

private[basis] object ShortMacros {
  import scala.reflect.macros.Context
  
  def MinValue(c: Context): c.Expr[Short] = c.literal(scala.Short.MinValue)
  
  def MaxValue(c: Context): c.Expr[Short] = c.literal(scala.Short.MaxValue)
}
