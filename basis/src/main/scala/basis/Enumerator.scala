/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

trait Enumerator[+A] extends Any {
  type Self <: Enumerator[A]
  
  protected def foreach[U](f: A => U): Unit
}

private[basis] object Enumerator {
  def traverse[A, U](self: Enumerator[A])(f: A => U): Unit = self.foreach[U](f)
}
