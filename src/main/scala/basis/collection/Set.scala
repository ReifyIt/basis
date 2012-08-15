/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Set[A] extends Any with Iterable[A] {
  override def iterator: Iterator[A]
  
  def contains(key: A): Boolean
}

object Set {
  import scala.language.implicitConversions
  
  @inline implicit def SetOps[A](self: Set[A]): SetOps[self.Kind, A] =
    new SetOps[self.Kind, A](self)
}
