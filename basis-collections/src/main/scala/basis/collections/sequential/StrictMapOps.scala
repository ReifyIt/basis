/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.traversable._

/** Strictly evaluated map operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
abstract class StrictMapOps[A, T, From] private[sequential]
