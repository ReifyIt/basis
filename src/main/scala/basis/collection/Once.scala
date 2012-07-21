/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Once[+A] extends Any {
  type From
  
  def foreach[U](f: A => U): Unit
}

object Once {
  import scala.language.implicitConversions
  
  @inline implicit def ForOnce[A](self: Once[A]): ForOnce[self.From, A] =
    new ForOnce[self.From, A](self)
  
  final class ForOnce[From, A](val __ : Once[A]) extends AnyVal {
    import __.foreach
    
    def select[B](q: PartialFunction[A, B]): Option[B] = {
      val f = new Analysis.Select(q)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def fold[B >: A](z: B)(op: (B, B) => B): B = {
      val f = new Analysis.FoldLeft(z)(op)
      foreach(f)
      f.result
    }
    
    def reduce[B >: A](op: (B, B) => B): B = {
      val f = new Analysis.ReduceLeft(op)
      foreach(f)
      if (f.isDefined) f.result else throw new UnsupportedOperationException
    }
    
    def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
      val f = new Analysis.ReduceLeft(op)
      foreach(f)
      if (f.isDefined) Some(f.result) else None
    }
    
    def find(p: A => Boolean): Option[A] = {
      val f = new Analysis.Find(p)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def forall(p: A => Boolean): Boolean = {
      val f = new Analysis.Forall(p)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def exists(p: A => Boolean): Boolean = {
      val f = new Analysis.Exists(p)
      try foreach(f) catch { case e: Break => () }
      f.result
    }
    
    def count(p: A => Boolean): Int = {
      val f = new Analysis.Count(p)
      foreach(f)
      f.result
    }
    
    def map[B](f: A => B)(implicit make: Make[From, B]): make.What = {
      foreach(new Analysis.MakeMap(f, make))
      make.result
    }
    
    def flatMap[B](f: A => Once[B])(implicit make: Make[From, B]): make.What = {
      foreach(new Analysis.MakeFlatMap(f, make))
      make.result
    }
    
    def filter(p: A => Boolean)(implicit make: Make[From, A]): make.What = {
      foreach(new Analysis.MakeFilter(p, make))
      make.result
    }
    
    def withFilter(p: A => Boolean): Once[A] = new WithFilter(__, p)
    
    def collect[B](q: PartialFunction[A, B])(implicit make: Make[From, B]): make.What = {
      foreach(new Analysis.MakeCollect(q, make))
      make.result
    }
  }
  
  private[basis] final class WithFilter[+A](self: Once[A], p: A => Boolean) extends Once[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(new Analysis.Filter(f, p))
  }
}
