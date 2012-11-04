/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A traversable collection of elements. Collection declares only a protected
  * `foreach` method; it has no public methods.
  * 
  * @groupprio  Examining   -2
  * @groupprio  Traversing  -1
  * 
  * @define collection  collection
  */
trait Collection[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Collection[A]] with Enumerator[A]
