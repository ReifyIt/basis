/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An equality relation. */
@scala.annotation.implicitNotFound("${T} has no implicit Equal implementation.")
trait Equal[-T] {
  /** Returns `true` given equal values and `false` given unequal ones. */
  def equal(x: T, y: T): Boolean
}

object Equal {
  /** Returns the given type's implicit `Equal` implementation. */
  def apply[T](implicit T: Equal[T]): T.type = T
}
