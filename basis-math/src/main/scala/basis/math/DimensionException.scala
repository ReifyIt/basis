/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** An exception indicating a dimension mismatch.
  * 
  * @author Chris Sachs
  * @group  Exceptions
  */
class DimensionException(message: String) extends java.lang.RuntimeException(message) {
  def this() = this(null)
}
