/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

object List {
  import scala.language.experimental.macros
  
  def apply[A](xs: A*)(implicit buffer: Buffer[List[A], A]): buffer.State =
    macro FactoryMacros.apply[A]
  
  def fill[A](n: Int)(element: => A)(implicit buffer: Buffer[List[A], A]): buffer.State =
    macro FactoryMacros.fill[A]
  
  def tabulate[A](n: Int)(f: Int => A)(implicit buffer: Buffer[List[A], A]): buffer.State =
    macro FactoryMacros.tabulate[A]
}
