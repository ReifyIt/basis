/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

/** A vector in an ''N''-dimensional `Real` vector space.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with a coordinate array.
  * @param  coordinates   The array of coordinates.
  * 
  * @define scalar  `Real` value
  */
final class VectorRN(protected val coordinates: Array[Double])
  extends RealVector[VectorRN] {
  
  /** Constructs a vector with repeated coordinate parameters.
    * 
    * @param  coordinates   the sequence of coordinates.
    */
  def this(coordinates: Double*) = this(coordinates.toArray[Double])
  
  /** The number of coordinates. */
  def dimension: Int = coordinates.length
  
  /** Returns the ''n''th coordinate.
    * 
    * @param  n   the zero-based coordinate index.
    * @return the $vector coordinate.
    */
  def apply(n: Int): Double = coordinates(n)
  
  def + (that: VectorRN): VectorRN = {
    if (dimension != that.dimension) throw new DimensionException
    val coordinates = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) + that(i)
      i += 1
    }
    new VectorRN(coordinates)
  }
  
  def unary_- : VectorRN = {
    val coordinates = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = -this(i)
      i += 1
    }
    new VectorRN(coordinates)
  }
  
  def - (that: VectorRN): VectorRN = {
    if (dimension != that.dimension) throw new DimensionException
    val coordinates = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) - that(i)
      i += 1
    }
    new VectorRN(coordinates)
  }
  
  def :* (scalar: Double): VectorRN = {
    val coordinates = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) * scalar
      i += 1
    }
    new VectorRN(coordinates)
  }
  
  def *: (scalar: Double): VectorRN =
    this :* scalar
  
  def / (scalar: Double): VectorRN = {
    val coordinates = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) / scalar
      i += 1
    }
    new VectorRN(coordinates)
  }
  
  /** Returns the dot product of this $vector and another $vector. The name of
    * this method uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: VectorRN): Real = {
    if (dimension != that.dimension || dimension == 0) throw new DimensionException
    var dp = 0.0
    var i = 0
    while (i < dimension) {
      dp += this(i) * that(i)
      i += 1
    }
    new Real(dp)
  }
  
  /** Returns the length (euclidean norm) of this $vector. */
  def length: Double = {
    if (dimension == 0) throw new DimensionException
    var dp = 0.0
    var i = 0
    while (i < dimension) {
      dp += this(i) * this(i)
      i += 1
    }
    math.sqrt(dp)
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorRN =>
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
    var h = -1131959659
    var i = 0
    while (i < dimension) {
      mix(h, this(i))
      i += 1
    }
    mash(h)
  }
  
  override def toString: String =
    coordinates.mkString("VectorRN(", ", ", ")")
  
  /** Converts this $vector to a coordinate sequence. */
  def toSeq: Seq[Double] = coordinates
}

/** Contains factory methods for ''N''-dimensional `Real` vectors. */
object VectorRN {
  def apply(coordinates: Array[Double]): VectorRN =
    new VectorRN(coordinates)
  
  def apply(coordinates: Double*): VectorRN =
    new VectorRN(coordinates: _*)
  
  def unapplySeq(vector: VectorRN): Some[Seq[Double]] =
    Some(vector.toSeq)
  
  /** The inner dot product typeclass for `RN`. */
  implicit val dotProduct = InnerProduct[VectorRN, Real](_ ⋅ _)
  
  /** The euclidean norm typeclass for `RN`. */
  implicit val euclideanNorm = Norm[VectorRN, Real](u => new Real(u.length))
}
