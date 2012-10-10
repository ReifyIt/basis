/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An iterable sequence of characters.
  * 
  * @author Chris Sachs
  * 
  * @define collection  string
  */
trait String extends Any with Seq[Char] {
  override type Self <: String
  
  override def iterator: StringIterator
}

/** A specialized iterator for character strings.
  * 
  * @define collection  string iterator
  */
trait StringIterator extends Any with Iterator[Char] {
  override def next(): Char
}
