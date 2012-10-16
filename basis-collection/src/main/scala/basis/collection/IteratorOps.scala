/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

class IteratorOps[A](self: Iterator[A]) {
  import scala.language.experimental.macros
  
  def foreach[U](f: A => U): Unit =
    macro IteratorMacros.foreach[A, U]
  
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro IteratorMacros.foldLeft[A, B]
  
  def reduce[B >: A](op: (B, B) => B): B =
    macro IteratorMacros.reduceLeft[A, B]
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro IteratorMacros.reduceLeftOption[A, B]
  
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro IteratorMacros.foldLeft[A, B]
  
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro IteratorMacros.reduceLeft[A, B]
  
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro IteratorMacros.reduceLeftOption[A, B]
  
  def find(p: A => Boolean): Option[A] =
    macro IteratorMacros.find[A]
  
  def forall(p: A => Boolean): Boolean =
    macro IteratorMacros.forall[A]
  
  def exists(p: A => Boolean): Boolean =
    macro IteratorMacros.exists[A]
  
  def count(p: A => Boolean): Int =
    macro IteratorMacros.count[A]
  
  def select[B](q: scala.PartialFunction[A, B]): Option[B] =
    macro IteratorMacros.select[A, B]
  
  def collect[B](q: scala.PartialFunction[A, B]): Iterator[B] =
    new Iterators.Collect(self.dup, q)
  
  def map[B](f: A => B): Iterator[B] =
    new Iterators.Map(self.dup, f)
  
  def flatMap[B](f: A => Iterator[B]): Iterator[B] =
    new Iterators.FlatMap(self.dup, f)
  
  def filter(p: A => Boolean): Iterator[A] =
    new Iterators.Filter(self.dup, p)
  
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)
  
  def dropWhile(p: A => Boolean): Iterator[A] =
    new Iterators.DropWhile(self.dup, p)
  
  def takeWhile(p: A => Boolean): Iterator[A] =
    new Iterators.TakeWhile(self.dup, p)
  
  def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))
  
  def drop(lower: Int): Iterator[A] =
    new Iterators.Drop(self.dup, lower)
  
  def take(upper: Int): Iterator[A] =
    new Iterators.Take(self.dup, upper)
  
  def slice(lower: Int, upper: Int): Iterator[A] =
    new Iterators.Slice(self.dup, lower, upper)
  
  def zip[B](that: Iterator[B]): Iterator[(A, B)] =
    new Iterators.Zip(self.dup, that.dup)
  
  def ++ [B >: A](that: Iterator[B]): Iterator[B] =
    new Iterators.++(self.dup, that.dup)
}
