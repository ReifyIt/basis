//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._

final class GeneralCollectionOps[+A](val __ : Collection[A]) extends AnyVal {
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = new GeneralTraverserOps(__).choose(q)
  def count(p: A => Boolean): Int                      = new GeneralTraverserOps(__).count(p)
  def exists(p: A => Boolean): Boolean                 = new GeneralTraverserOps(__).exists(p)
  def find(p: A => Boolean): Maybe[A]                  = new GeneralTraverserOps(__).find(p)
  def fold[B >: A](z: B)(op: (B, B) => B): B           = new GeneralTraverserOps(__).fold(z)(op)
  def foldLeft[B](z: B)(op: (B, A) => B): B            = new GeneralTraverserOps(__).foldLeft(z)(op)
  def forall(p: A => Boolean): Boolean                 = new GeneralTraverserOps(__).forall(p)
  def foreach[U](f: A => U): Unit                      = new GeneralTraverserOps(__).foreach(f)
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = new GeneralTraverserOps(__).mayReduce(op)
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = new GeneralTraverserOps(__).mayReduceLeft(op)
  def reduce[B >: A](op: (B, B) => B): B               = new GeneralTraverserOps(__).reduce(op)
  def reduceLeft[B >: A](op: (B, A) => B): B           = new GeneralTraverserOps(__).reduceLeft(op)

  def eagerly: StrictCollectionOps[A, Collection[_]]   = new StrictCollectionOps[A, Collection[_]](__)
  def lazily: NonStrictCollectionOps[A]                = new NonStrictCollectionOps[A](__)
}
