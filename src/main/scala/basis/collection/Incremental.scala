/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Incremental[+A] extends Any {
  def foreach[U](f: A => U): Unit
  
  def collectFirst[B](q: PartialFunction[A, B]): Option[B]
  
  def fold[B >: A](z: B)(op: (B, B) => B): B
  
  def reduce[B >: A](op: (B, B) => B): B
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B]
  
  def foldLeft[B](z: B)(op: (B, A) => B): B
  
  def reduceLeft[B >: A](op: (B, A) => B): B
  
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]
  
  def find(p: A => Boolean): Option[A]
  
  def forall(p: A => Boolean): Boolean
  
  def exists(p: A => Boolean): Boolean
  
  def count(p: A => Boolean): Int
}
