/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package strict

import traversable._

/** Strictly evaluated collection operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  collection
  */
trait CollectionOps[+A, +From] extends Any with EnumeratorOps[A, From] with general.CollectionOps[A, From]
