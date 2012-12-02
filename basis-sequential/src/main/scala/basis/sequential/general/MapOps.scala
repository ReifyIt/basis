/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package general

import basis.collections._
import basis.collections.traversable._

/** General map operations.
  * 
  * @groupprio  Traversing  -3
  * @groupprio  Reducing    -2
  * @groupprio  Querying    -1
  */
final class MapOps[+A, +T]
