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
  * @tparam AffinePoint   the point type of the affine space.
  * @tparam Vector        the vector type of the affine space.
  * @tparam Scalar        the scalar type of the affine space.
  * 
  * @define point   point
  * @define vector  vector
  * @define scalar  scalar
  */
trait AffinePoint[AffinePoint, Vector, -Scalar] {
  /** Adds a $vector to this $point.
    * 
    * @param  vector  the $vector to add.
    * @return the affine sum of this $point and the other $vector.
    */
  def :+ (vector: Vector): AffinePoint
  
  /** Subtracts a $vector from this $point.
    * 
    * @param  vector  the $vector to subtract.
    * @return the affine difference of this $point and the other $vector.
    */
  def :- (vector: Vector): AffinePoint
  
  /** Subtracts a $point from this $point.
    * 
    * @param  that  the $point to subtract.
    * @return the affine difference of this $point and the other $point.
    */
  def - (that: AffinePoint): Vector
}
