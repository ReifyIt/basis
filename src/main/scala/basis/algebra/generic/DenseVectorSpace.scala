/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class DenseVectorSpace[S <: Field { type Scalar = S }]
    (override val Scalar: ScalarSpace { type Scalar = S })
    (dimension: Int)
  extends DenseVectorModule[S](Scalar)(dimension) with LinearSpace
