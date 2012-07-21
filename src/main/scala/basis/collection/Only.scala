/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Only[A] extends Any with Many[A] {
  override def iterator: Next[A]
  
  def contains(key: A): Boolean
}

object Only {
  import scala.language.implicitConversions
  
  @inline implicit def ForOnly[A](self: Only[A]): ForOnly[self.From, A] =
    new ForOnly[self.From, A](self)
  
  final class ForOnly[From, A](val __ : Only[A]) extends AnyVal {
    
  }
}
