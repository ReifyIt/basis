/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

private[basis] object Readers {
  import scala.annotation.tailrec
  
  final class ++ private (xs: Reader, ys: Reader, private[this] var segment: Int) extends Reader {
    def this(xs: Reader, ys: Reader) = this(xs, ys, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => xs.isEmpty && { segment = 1; isEmpty }
      case 1 => ys.isEmpty
    }
    
    @tailrec override def head: Char = segment match {
      case 0 => if (!xs.isEmpty) xs.head else { segment = 1; head }
      case 1 => ys.head
    }
    
    @tailrec override def step(): Unit = segment match {
      case 0 => if (!xs.isEmpty) xs.step else { segment = 1; step() }
      case 1 => ys.step()
    }
    
    override def dup: Reader = segment match {
      case 0 if !xs.isEmpty => new ++ (xs.dup, ys.dup, 0)
      case _ => ys.dup
    }
  }
}
