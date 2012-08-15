/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Map[A, +Z] extends Any with Iterable[(A, Z)] {
  override def iterator: Iterator[(A, Z)]
  
  def get(key: A): Option[Z]
}

object Map {
  import scala.language.implicitConversions
  
  @inline implicit def MapOps[A, Z](self: Map[A, Z]): MapOps[self.Kind, A, Z] =
    new MapOps[self.Kind, A, Z](self)
}
