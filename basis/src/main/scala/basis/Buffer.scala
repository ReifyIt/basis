/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

trait Buffer[-Source, -A] {
  type State
  
  def += (x: A): this.type
  
  def expect(n: Int): this.type
  
  def check: State
  
  def clear(): scala.Unit
}
