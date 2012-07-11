/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Collector[-Scope, -A] {
  type Product
  
  def expect(count: Int): Unit
  
  def += (element: A): Unit
  
  def ++= (elements: Incremental[A]) {
    for (element <- elements) this += element
  }
  
  def result: Product
  
  def clear(): Unit
}

object Collector {
  def nextSize(base: Int, size: Int): Int = {
    var n = math.max(base, size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}
