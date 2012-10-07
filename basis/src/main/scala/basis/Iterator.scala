/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

trait Iterator[+A] extends Any with Enumerator[A] {
  override type Self <: Iterator[A]
  
  def hasNext: Boolean
  
  def next(): A
  
  protected override def foreach[U](f: A => U): Unit =
    while (hasNext) f(next())
}
