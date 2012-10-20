/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collection._

/** A specialized iterable sequence of Unicode® characters.
  * 
  * @author Chris Sachs
  * 
  * @define collection  rope
  */
trait Rope extends Any with Seq[Char] {
  override type Self <: Rope
  
  override def iterator: Reader
}
