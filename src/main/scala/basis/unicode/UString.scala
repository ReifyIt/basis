/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.unicode

import basis.collection._

/** A Unicode® string comprised of a sequence of code units.
  * 
  * @author Chris Sachs
  */
trait UString extends Any with Iterable[Int] {
  /** Returns the number of code units in this Unicode string. */
  def length: Int
  
  /** Returns the code unit at the specified index;
    * '''DOES NOT''' decode a character at that index. */
  def apply(index: Int): Int
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Implementations define the handling of ill-formed code unit sequences. */
  override def foreach[U](f: Int => U): Unit
  
  override def iterator: UStringCursor
}
