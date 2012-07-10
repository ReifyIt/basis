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
  
  def result(): Product
  
  def clear(): Unit
}
