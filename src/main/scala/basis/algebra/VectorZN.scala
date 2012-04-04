/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

/** A vector in an ''N''-dimensional `Integer` module.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with a coordinate array.
  * @param  coordinates   The array of coordinates.
  * 
  * @define scalar  `Integer` value
  */
final class VectorZN(protected val coordinates: Array[Long])
  extends IntegerVector[VectorZN] {
  
  /** Constructs a vector with repeated coordinate parameters.
    * 
    * @param  coordinates   the sequence of coordinates.
    */
  def this(coordinates: Long*) = this(coordinates.toArray[Long])
  
  /** The number of coordinates. */
  def dimension: Int = coordinates.length
  
  /** Returns the ''n''th coordinate.
    * 
    * @param  n   the zero-based coordinate index.
    * @return the $vector coordinate.
    */
  def apply(n: Int): Long = coordinates(n)
  
  def + (that: VectorZN): VectorZN = {
    if (dimension != that.dimension) throw new DimensionException
    val coordinates = new Array[Long](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) + that(i)
      i += 1
    }
    new VectorZN(coordinates)
  }
  
  def unary_- : VectorZN = {
    val coordinates = new Array[Long](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = -this(i)
      i += 1
    }
    new VectorZN(coordinates)
  }
  
  def - (that: VectorZN): VectorZN = {
    if (dimension != that.dimension) throw new DimensionException
    val coordinates = new Array[Long](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) - that(i)
      i += 1
    }
    new VectorZN(coordinates)
  }
  
  def :* (scalar: Long): VectorZN = {
    val coordinates = new Array[Long](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = this(i) * scalar
      i += 1
    }
    new VectorZN(coordinates)
  }
  
  def *: (scalar: Long): VectorZN =
    this :* scalar
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorZN =>
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
    var h = -1131959411
    var i = 0
    while (i < dimension) {
      mix(h, this(i))
      i += 1
    }
    mash(h)
  }
  
  override def toString: String =
    coordinates.mkString("VectorZN(", ", ", ")")
  
  /** Converts this $vector to a coordinate sequence. */
  def toSeq: Seq[Long] = coordinates
}

/** Contains factory methods for ''N''-dimensional `Integer` vectors. */
object VectorZN {
  def apply(coordinates: Array[Long]): VectorZN =
    new VectorZN(coordinates)
  
  def apply(coordinates: Long*): VectorZN =
    new VectorZN(coordinates: _*)
  
  def unapplySeq(vector: VectorZN): Some[Seq[Long]] =
    Some(vector.toSeq)
}
