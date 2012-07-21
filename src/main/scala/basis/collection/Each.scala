/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Each[+A] extends Any with Once[A] {
  override def foreach[U](f: A => U): Unit
  
  protected def stringPrefix: String = {
    var name = getClass.getName
    val dot = name.lastIndexOf('.')
    (if (dot < 0) name else name.substring(dot + 1)).replace('$', '.')
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix).append('(')
    foreach(new Analysis.AddString(s, ", "))
    s.append(')').toString
  }
}

object Each {
  import scala.language.implicitConversions
  
  @inline implicit def ForEach[A](self: Each[A]): ForEach[self.From, A] =
    new ForEach[self.From, A](self)
  
  final class ForEach[From, A](val __ : Each[A]) extends AnyVal {
    import __.foreach
    
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
    
    def withFilter(p: A => Boolean): Each[A] = new WithFilter(__, p)
    
    def collect[B](q: PartialFunction[A, B])(implicit make: Make[From, B]): make.What = {
      foreach(new Analysis.MakeCollect(q, make))
      make.result
    }
  }
  
  private[basis] final class WithFilter[+A](self: Each[A], p: A => Boolean) extends Each[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(new Analysis.Filter(f, p))
  }
}
