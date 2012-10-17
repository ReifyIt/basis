/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A producer of textual object descriptions. */
@scala.annotation.implicitNotFound("${T} has no implicit Show implementation.")
trait Show[-T] {
  /** Appends a description of the given value to a character buffer. */
  def show(x: T)(implicit buffer: CharBuffer): Unit
}

object Show {
  /** Returns the given type's implicit `Show` implementation. */
  def apply[T](implicit T: Show[T]): T.type = T
}
