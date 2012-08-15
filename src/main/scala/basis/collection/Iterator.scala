/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterator[+A] extends Any with Traversable[A] {
  def hasNext: Boolean
  
  def next(): A
  
  override def foreach[U](f: A => U): Unit = while (hasNext) f(next())
}

object Iterator {
  import scala.language.implicitConversions
  
  @inline implicit def IteratorOps[A](self: Iterator[A]): IteratorOps[self.Kind, A] =
    new IteratorOps[self.Kind, A](self)
  
  object Empty extends Iterator[Nothing] {
    override def hasNext: Boolean = false
    override def next(): Nothing = throw new NoSuchElementException("next to nada")
  }
}
