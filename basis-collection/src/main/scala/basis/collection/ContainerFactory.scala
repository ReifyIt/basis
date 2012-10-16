/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

import scala.language.higherKinds

trait ContainerFactory[CC[X] <: Container[X]] {
  import scala.language.experimental.macros
  
  def apply[A](xs: A*)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro ContainerMacros.apply[A]
  
  def fill[A](n: Int)(element: => A)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro ContainerMacros.fill[A]
  
  def tabulate[A](n: Int)(f: Int => A)(implicit buffer: Buffer[CC[_], A]): buffer.State =
    macro ContainerMacros.tabulate[A]
  
  implicit def Show[A : Show]: Show[CC[A]] = new ContainerShow[A](stringPrefix)
  
  protected def stringPrefix: String
}
