/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.stat

/** Failure to generate an arbitrary value.
  *
  * @author Chris Sachs
  * @since  0.0
  */
class ArbitraryException(message: String) extends RuntimeException(message)
