/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Onto[A, +Z] extends Any with Many[(A, Z)] {
  override def iterator: Next[(A, Z)]
  
  def get(key: A): Option[Z]
}

object Onto {
  import scala.language.implicitConversions
  
  @inline implicit def ForOnto[A, Z](self: Onto[A, Z]): ForOnto[self.Self, A, Z] =
    new ForOnto[self.Self, A, Z](self)
  
  final class ForOnto[From, A, Z](val __ : Onto[A, Z]) extends AnyVal {
    
  }
}
