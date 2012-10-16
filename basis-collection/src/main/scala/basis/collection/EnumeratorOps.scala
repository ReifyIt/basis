/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

class EnumeratorOps[+Self, +A](self: Enumerator[A]) {
  import Enumerator.traverse
  
  def foreach[U](f: A => U): Unit = traverse(self)(f)
  
  def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val f = new Traversers.FoldLeft(z)(op)
    traverse(self)(f)
    f.state
  }
  
  def reduce[B >: A](op: (B, B) => B): B = {
    val f = new Traversers.ReduceLeft(op)
    traverse(self)(f)
    if (f.isDefined) f.state else throw new java.lang.UnsupportedOperationException
  }
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    val f = new Traversers.ReduceLeft(op)
    traverse(self)(f)
    if (f.isDefined) Some(f.state) else None
  }
  
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val f = new Traversers.FoldLeft(z)(op)
    traverse(self)(f)
    f.state
  }
  
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val f = new Traversers.ReduceLeft(op)
    traverse(self)(f)
    if (f.isDefined) f.state else throw new java.lang.UnsupportedOperationException
  }
  
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    val f = new Traversers.ReduceLeft(op)
    traverse(self)(f)
    if (f.isDefined) Some(f.state) else None
  }
  
  def find(p: A => Boolean): Option[A] = {
    val f = new Traversers.Find(p)
    try traverse(self)(f) catch { case e: Break => () }
    f.state
  }
  
  def forall(p: A => Boolean): Boolean = {
    val f = new Traversers.Forall(p)
    try traverse(self)(f) catch { case e: Break => () }
    f.state
  }
  
  def exists(p: A => Boolean): Boolean = {
    val f = new Traversers.Exists(p)
    try traverse(self)(f) catch { case e: Break => () }
    f.state
  }
  
  def count(p: A => Boolean): Int = {
    val f = new Traversers.Count(p)
    traverse(self)(f)
    f.state
  }
  
  def select[B](q: scala.PartialFunction[A, B]): Option[B] = {
    val f = new Traversers.Select(q)
    try traverse(self)(f) catch { case e: Break => () }
    f.state
  }
  
  def collect[B](q: scala.PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(self)(new Traversers.CollectInto(q, buffer))
    buffer.state
  }
  
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(self)(new Traversers.MapInto(f, buffer))
    buffer.state
  }
  
  def flatMap[B](f: A => Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(self)(new Traversers.FlatMapInto(f, buffer))
    buffer.state
  }
  
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(self)(new Traversers.FilterInto(p, buffer))
    buffer.state
  }
  
  def withFilter(p: A => Boolean): Enumerator[A] =
    new EnumeratorWithFilter(self, p)
}

private[basis] final class EnumeratorWithFilter[+A](self: Enumerator[A], p: A => Boolean) extends Enumerator[A] {
  protected override def foreach[U](f: A => U): Unit =
    Enumerator.traverse(self)(new Traversers.Filter(p, f))
}
