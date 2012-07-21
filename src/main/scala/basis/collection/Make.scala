/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Make[-From, -A] extends Any {
  type What
  
  def expect(count: Int): Unit
  
  def += (that: A): Unit
  
  def ++= (those: Once[A]): Unit = those.foreach(new Analysis.Maker(this))
  
  def result: What
  
  def clear(): Unit
}

object Make {
  def nextSize(base: Int, size: Int): Int = {
    var n = math.max(base, size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}
