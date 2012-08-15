/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traversable[+A] extends Any {
  type Kind
  
  def foreach[U](f: A => U): Unit
}

object Traversable {
  import scala.language.implicitConversions
  
  @inline implicit def TraversableOps[A](self: Traversable[A]): TraversableOps[self.Kind, A] =
    new TraversableOps[self.Kind, A](self)
}
