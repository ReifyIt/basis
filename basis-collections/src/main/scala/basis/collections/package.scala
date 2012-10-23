/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object collections {
  /** Applies a function to each of an enumerator's elements by invoking the
    * enumerator's protected `foreach` method. */
  def traverse[A, U](xs: Enumerator[A])(f: A => U): Unit =
    Enumerator.traverse[A, U](xs)(f)
  
  private[basis] final class Break extends java.lang.Throwable
  private[basis] val Break = new Break
}
