/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterable[+A] extends Any with Traversable[A] {
  def iterator: Iterator[A]
  
  override def foreach[U](f: A => U): Unit = iterator.foreach(f)
  
  override def collectFirst[B](q: PartialFunction[A, B]): Option[B] = iterator.collectFirst(q)
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B = iterator.foldLeft(z)(op)
  
  override def reduceLeft[B >: A](op: (B, A) => B): B = iterator.reduceLeft(op)
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = iterator.reduceLeftOption(op)
  
  override def find(p: A => Boolean): Option[A] = iterator.find(p)
  
  override def forall(p: A => Boolean): Boolean = iterator.forall(p)
  
  override def exists(p: A => Boolean): Boolean = iterator.exists(p)
  
  override def count(p: A => Boolean): Int = iterator.count(p)
  
  override def eagerly: Iterating[Any, A] = new Iterating.Projecting[Any, A](this)
  
  override def lazily: Iterates[A] = new Iterates.Projects[A](this)
}

object Iterable {
  abstract class Abstractly[+A] extends Traversable.Abstractly[A] with Iterable[A]
}
