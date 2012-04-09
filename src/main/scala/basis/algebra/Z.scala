/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

class Z(val dimension: Int) extends Module { ZN =>
  type Scalar = Integer
  
  final class Vector(private val coordinates: Array[Long])
    extends IntegerVector[Vector] {
    
    if (dimension != ZN.dimension)
      throw new DimensionException(dimension.toString)
    
    def dimension: Int = coordinates.length
    
    def apply(n: Int): Long = coordinates(n)
    
    def + (that: Vector): Vector = {
      if (dimension != that.dimension) throw new DimensionException
      val coordinates = new Array[Long](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) + that(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def unary_- : Vector = {
      val coordinates = new Array[Long](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = -this(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def - (that: Vector): Vector = {
      if (dimension != that.dimension) throw new DimensionException
      val coordinates = new Array[Long](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) - that(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def :* (scalar: Long): Vector = {
      val coordinates = new Array[Long](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) * scalar
        i += 1
      }
      new Vector(coordinates)
    }
    
    def *: (scalar: Long): Vector = this :* scalar
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector =>
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
      var h = -410134227
      var i = 0
      while (i < dimension) {
        mix(h, this(i))
        i += 1
      }
      mash(h)
    }
    
    override def toString: String = {
      val s = new StringBuilder
      s.append('<')
      if (0 < dimension) s.append(this(0))
      var i = 1
      while (i < dimension) {
        s.append(", ").append(this(i))
        i += 1
      }
      s.append('>')
      s.toString
    }
    
    private[Z] def toSeq: Seq[Long] = wrapLongArray(coordinates)
  }
  
  val Scalar = Integer
  
  lazy val zero: Vector = new Vector(new Array[Long](dimension))
  
  def apply(coordinates: Long*): Vector =
    new Vector(coordinates.toArray[Long])
  
  def unapply(vector: Vector): Option[Seq[Long]] =
    if (vector.dimension == dimension) Some(vector.toSeq) else None
  
  override def toString: String = "Z"+"("+ dimension +")"
}
