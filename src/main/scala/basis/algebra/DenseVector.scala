/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

/** A dense generic vector over a ring.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with a coordinate array.
  * @tparam Scalar        the scalar type of the vector.
  * @param  coordinates   The array of coordinates.
  * 
  * @define vector  dense vector
  */
final class DenseVector[Scalar <: Ring[Scalar]](protected val coordinates: Array[AnyRef])
  extends Vector[DenseVector[Scalar], Scalar] {
  
  /** Constructs a vector with repeated coordinate parameters.
    * 
    * @param  coordinates   the sequence of coordinates.
    */
  def this(coordinates: Scalar*) = this(coordinates.toArray[AnyRef])
  
  /** The number of coordinates. */
  def dimension: Int = coordinates.length
  
   /** Returns the ''n''th coordinate.
    * 
    * @param  n   the zero-based coordinate index.
    * @return the $vector coordinate.
    */
  def apply(n: Int): Scalar = coordinates(n).asInstanceOf[Scalar]
  
  def + (that: DenseVector[Scalar]): DenseVector[Scalar] = {
    if (dimension != that.dimension) throw new DimensionException
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) + that(i)
      i += 1
    }
    new DenseVector[Scalar](coordinates)
  }
  
  def unary_- : DenseVector[Scalar] = {
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = -this(i)
      i += 1
    }
    new DenseVector[Scalar](coordinates)
  }
  
  def - (that: DenseVector[Scalar]): DenseVector[Scalar] = {
    if (dimension != that.dimension) throw new DimensionException
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) - that(i)
      i += 1
    }
    new DenseVector[Scalar](coordinates)
  }
  
  def :* (scalar: Scalar): DenseVector[Scalar] = {
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) * scalar
      i += 1
    }
    new DenseVector[Scalar](coordinates)
  }
  
  def *: (scalar: Scalar): DenseVector[Scalar] = {
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = scalar * this(i)
      i += 1
    }
    new DenseVector[Scalar](coordinates)
  }
  
  /** Divides this $vector by a $scalar.
    * 
    * @param  scalar    the $scalar to divide by.
    * @param  isField   implicit evidence that `Scalar` is a `Field`.
    * @return the scaled $vector.
    */
  def / (scalar: Scalar)(implicit isField: Scalar <:< Field[Scalar]): DenseVector[Scalar] = {
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) / scalar
      i += 1
    }
    new DenseVector[Scalar](coordinates)
  }
  
  /** Returns the dot product of this $vector and another $vector. The name of
    * this method uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: DenseVector[Scalar]): Scalar = {
    if (dimension != that.dimension || dimension == 0) throw new DimensionException
    var dp = this(0) * that(0)
    var i = 1
    while (i < dimension) {
      dp += this(i) * that(i)
      i += 1
    }
    dp
  }
  
  /** Returns the euclidean norm of this $vector.
    * 
    * @param  isCompleteField   implicit evidence that `Scalar` is a `CompleteField`.
    * @return the square root of the dot product of this $vector with itself.
    */
  def norm(implicit isCompleteField: Scalar <:< CompleteField[Scalar]): Scalar = {
    if (dimension == 0) throw new DimensionException
    var dp = this(0) * this(0)
    var i = 1
    while (i < dimension) {
      dp += this(i) * this(i)
      i += 1
    }
    dp.sqrt
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: DenseVector[_] =>
      var equal = dimension == that.dimension
      var i = 0
      while (i < dimension && equal) {
        equal = this(i) == that(i)
        i += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    var h = -1360010836
    var i = 0
    while (i < dimension) {
      mix(h, this(i))
      i += 1
    }
    mash(h)
  }
  
  override def toString: String =
    coordinates.mkString("DenseVector(", ", ", ")")
  
  /** Converts this $vector to a coordinate sequence. */
  def toSeq: Seq[Scalar] = (coordinates: Seq[AnyRef]).asInstanceOf[Seq[Scalar]]
}

/** Contains factory methods for dense generic vectors. */
object DenseVector {
  def apply[Scalar <: Ring[Scalar]](coordinates: Scalar*): DenseVector[Scalar] =
    new DenseVector[Scalar](coordinates: _*)
  
  def unapplySeq[Scalar <: Ring[Scalar]](vector: DenseVector[Scalar]): Some[Seq[Scalar]] =
    Some(vector.toSeq)
}
