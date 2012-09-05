/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Builder[-Kind, -A] extends Any {
  type Result
  
  def expect(count: Int): Unit
  
  def += (that: A): Unit
  
  def ++= (those: Traversable[A]): Unit =
    those.foreach(new TraversableOps.Append(this))
  
  def result: Result
  
  def clear(): Unit
}

object Builder {
  def expand(base: Int, size: Int): Int = {
    var n = math.max(base, size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}
