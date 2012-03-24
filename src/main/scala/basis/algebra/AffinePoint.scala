/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A point in an affine space.
  * 
  * @author Chris Sachs
  * 
  * @define point   point
  * @define vector  vector
  * @define scalar  scalar
  * 
  * @tparam AffinePoint   the `Point` type of the affine space.
  * @tparam Vector        the `Vector` type of the affine space.
  * @tparam Scalar        the `Scalar` type of the affine space.
  */
trait AffinePoint[AffinePoint, Vector, -Scalar] {
  /** Adds a $vector to this $point.
    * 
    * @param  vector  the $vector to add.
    * @return the affine sum of this $point and another $vector.
    */
  def :+ (vector: Vector): AffinePoint
  
  /** Subtracts a $vector from this $point.
    * 
    * @param  vector  the $vector to subtract.
    * @return the affine difference of this $point and another $vector.
    */
  def :- (vector: Vector): AffinePoint
  
  /** Subtracts a $point from this $point.
    * 
    * @param  that  the $point to subtract.
    * @return the affine difference of this $point and another $point.
    */
  def - (that: AffinePoint): Vector
}
