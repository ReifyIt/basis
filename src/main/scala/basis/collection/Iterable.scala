/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterable[+A] extends Any with Traversable[A] {
  def iterator: Iterator[A]
  
  override def foreach[U](f: A => U) {
    val iter = iterator
    while (iter.hasNext) f(iter.next())
  }
  
  protected def stringPrefix: String = {
    var name = getClass.getName
    val dot = name.lastIndexOf('.')
    (if (dot < 0) name else name.substring(dot + 1)).replace('$', '.')
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix).append('(')
    val iter = iterator
    if (iter.hasNext) s.append(iter.next())
    while (iter.hasNext) s.append(", ").append(iter.next())
    s.append(')').toString
  }
}

object Iterable {
  import scala.language.implicitConversions
  
  @inline implicit def IterableOps[A](self: Iterable[A]): IterableOps[self.Kind, A] =
    new IterableOps[self.Kind, A](self)
}
