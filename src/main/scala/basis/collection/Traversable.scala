/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traversable[+A] extends Any with Incremental[A] {
  override def foreach[U](f: A => U): Unit
  
  override def collectFirst[B](q: PartialFunction[A, B]): Option[B] = {
    var result = None: Option[B]
    try for (x <- this) if (q.isDefinedAt(x)) { result = Some(q(x)); throw Break }
    catch { case e: Break => () }
    result
  }
  
  override def fold[B >: A](z: B)(op: (B, B) => B): B = foldLeft(z)(op)
  
  override def reduce[B >: A](op: (B, B) => B): B = reduceLeft(op)
  
  override def reduceOption[B >: A](op: (B, B) => B): Option[B] = reduceLeftOption(op)
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this) result = op(result, x)
    result
  }
  
  override def reduceLeft[B >: A](op: (B, A) => B): B = {
    var result = null.asInstanceOf[B]
    var first = true
    for (x <- this) if (first) { result = x; first = false } else result = op(result, x)
    if (!first) result else throw new UnsupportedOperationException("empty reduce")
  }
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    var result = null.asInstanceOf[B]
    var first = true
    for (x <- this) if (first) { result = x; first = false } else result = op(result, x)
    if (!first) Some(result) else None
  }
  
  override def find(p: A => Boolean): Option[A] = {
    var result = None: Option[A]
    try for (x <- this) if (p(x)) { result = Some(x); throw Break }
    catch { case e: Break => () }
    result
  }
  
  override def forall(p: A => Boolean): Boolean = {
    var result = true
    try for (x <- this) if (!p(x)) { result = false; throw Break }
    catch { case e: Break => () }
    result
  }
  
  override def exists(p: A => Boolean): Boolean = {
    var result = false
    try for (x <- this) if (p(x)) { result = true; throw Break }
    catch { case e: Break => () }
    result
  }
  
  override def count(p: A => Boolean): Int = {
    var total = 0
    for (x <- this) if (p(x)) total += 1
    total
  }
  
  def view: TraversableView[A] = new TraversableView.Projected[A](this)
}

private[basis] abstract class AbstractTraversable[+A] extends Traversable[A]
