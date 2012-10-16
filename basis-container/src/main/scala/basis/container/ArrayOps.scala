/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class ArrayOps[+Self, +A](self: Array[A]) {
  import scala.language.experimental.macros
  
  def foreach[U](f: A => U): Unit =
    macro ArrayMacros.foreach[A, U]
  
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro ArrayMacros.foldLeft[A, B]
  
  def reduce[B >: A](op: (B, B) => B): B =
    macro ArrayMacros.reduceLeft[A, B]
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro ArrayMacros.reduceLeftOption[A, B]
  
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro ArrayMacros.foldLeft[A, B]
  
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro ArrayMacros.reduceLeft[A, B]
  
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro ArrayMacros.reduceLeftOption[A, B]
  
  def foldRight[B](z: B)(op: (A, B) => B): B =
    macro ArrayMacros.foldRight[A, B]
  
  def reduceRight[B >: A](op: (A, B) => B): B =
    macro ArrayMacros.reduceRight[A, B]
  
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] =
    macro ArrayMacros.reduceRightOption[A, B]
  
  def find(p: A => Boolean): Option[A] =
    macro ArrayMacros.find[A]
  
  def forall(p: A => Boolean): Boolean =
    macro ArrayMacros.forall[A]
  
  def exists(p: A => Boolean): Boolean =
    macro ArrayMacros.exists[A]
  
  def count(p: A => Boolean): Int =
    macro ArrayMacros.count[A]
  
  def select[B](q: scala.PartialFunction[A, B]): Option[B] =
    macro ArrayMacros.select[A, B]
  
  def collect[B](q: scala.PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ArrayMacros.collect[A, B]
  
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ArrayMacros.map[A, B]
  
  def flatMap[B](f: A => Array[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ArrayMacros.flatMap[A, B]
  
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.filter[A]
  
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.dropWhile[A]
  
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.takeWhile[A]
  
  /* FIXME: SI-6447 */
  //def span(p: A => Boolean)(
  //    implicit builderA: Buffer[Self, A],
  //             builderB: Buffer[Self, A])
  //  : (builderA.State, builderB.State) =
  //  macro ArrayMacros.span[A]
  
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.drop[A]
  
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.take[A]
  
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.slice[A]
  
  def reverse(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ArrayMacros.reverse[A]
  
  def ++ [B >: A](that: Array[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ArrayMacros.++[A, B]
}
