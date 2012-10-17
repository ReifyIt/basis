/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

class SeqOps[+Self, +A](self: Seq[A]) {
  def foreach[U](f: A => U): Unit =
    macro ContainerMacros.foreach[A, U]
  
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro ContainerMacros.foldLeft[A, B]
  
  def reduce[B >: A](op: (B, B) => B): B =
    macro ContainerMacros.reduceLeft[A, B]
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro ContainerMacros.reduceLeftOption[A, B]
  
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro ContainerMacros.foldLeft[A, B]
  
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro ContainerMacros.reduceLeft[A, B]
  
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro ContainerMacros.reduceLeftOption[A, B]
  
  def find(p: A => Boolean): Option[A] =
    macro ContainerMacros.find[A]
  
  def forall(p: A => Boolean): Boolean =
    macro ContainerMacros.forall[A]
  
  def exists(p: A => Boolean): Boolean =
    macro ContainerMacros.exists[A]
  
  def count(p: A => Boolean): Int =
    macro ContainerMacros.count[A]
  
  def select[B](q: scala.PartialFunction[A, B]): Option[B] =
    macro ContainerMacros.select[A, B]
  
  def collect[B](q: scala.PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.collect[A, B]
  
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.map[A, B]
  
  def flatMap[B](f: A => Container[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.flatMap[A, B]
  
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.filter[A]
  
  def withFilter(p: A => Boolean): Container[A] =
    new ContainerWithFilter(self, p)
  
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.dropWhile[A]
  
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.takeWhile[A]
  
  /* FIXME: SI-6447 */
  //def span(p: A => Boolean)(
  //    implicit builderA: Buffer[Self, A],
  //             builderB: Buffer[Self, A])
  //  : (builderA.State, builderB.State) =
  //  macro ContainerMacros.span[A]
  
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.drop[A]
  
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.take[A]
  
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.slice[A]
  
  def zip[B](that: Container[B])(implicit buffer: Buffer[Self, (A, B)]): buffer.State =
    macro ContainerMacros.zip[A, B]
  
  def ++ [B >: A](that: Container[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.++[A, B]
}
