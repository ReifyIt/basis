/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

final class ListOps[+A, +Self](self: List[A]) {
  import scala.language.experimental.macros
  
  def foreach[U](f: A => U): Unit =
    macro ListMacros.foreach[A, U]
  
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro ListMacros.foldLeft[A, B]
  
  def reduce[B >: A](op: (B, B) => B): B =
    macro ListMacros.reduceLeft[A, B]
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro ListMacros.reduceLeftOption[A, B]
  
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro ListMacros.foldLeft[A, B]
  
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro ListMacros.reduceLeft[A, B]
  
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro ListMacros.reduceLeftOption[A, B]
  
  def find(p: A => Boolean): Option[A] =
    macro ListMacros.find[A]
  
  def forall(p: A => Boolean): Boolean =
    macro ListMacros.forall[A]
  
  def exists(p: A => Boolean): Boolean =
    macro ListMacros.exists[A]
  
  def count(p: A => Boolean): Int =
    macro ListMacros.count[A]
  
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro ListMacros.select[A, B]
  
  def collect[B](q: PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ListMacros.collect[A, B]
  
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ListMacros.map[A, B]
  
  def flatMap[B](f: A => List[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ListMacros.flatMap[A, B]
  
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ListMacros.filter[A]
  
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ListMacros.dropWhile[A]
  
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ListMacros.takeWhile[A]
  
  def span(p: A => Boolean)(
      implicit builderA: Buffer[Self, A],
               builderB: Buffer[Self, A])
    : (builderA.State, builderB.State) =
    macro ListMacros.span[A]
  
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ListMacros.drop[A]
  
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ListMacros.take[A]
  
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ListMacros.slice[A]
  
  def ++ [B >: A](that: List[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ListMacros.++[A, B]
}
