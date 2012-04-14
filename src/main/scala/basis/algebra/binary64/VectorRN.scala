/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait VectorRN extends VectorFN { self =>
  override type Vector >: self.type <: VectorRN {
    type Vector = self.Vector
  }
  
  override type Scalar = Real
  
  override def Space: RN {
    type Vector = self.Vector
  }
  
  def apply(i: Int): Double
  
  def coord(i: Int): Real = new Real(this(i))
  
  override def + (that: Vector): Vector = {
    if (dimension != that.dimension)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val coords = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = this(i) + that(i)
      i += 1
    }
    Space(coords)
  }
  
  override def unary_- : Vector = {
    val coords = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = -this(i)
      i += 1
    }
    Space(coords)
  }
  
  override def - (that: Vector): Vector = {
    if (dimension != that.dimension)
      throw new DimensionException(Space.toString +" - "+ that.Space.toString)
    val coords = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = this(i) - that(i)
      i += 1
    }
    Space(coords)
  }
  
  override def :* (scalar: Real): Vector = this :* scalar.toDouble
  
  def :* (scalar: Double): Vector = {
    val coords = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = this(i) * scalar
      i += 1
    }
    Space(coords)
  }
  
  override def *: (scalar: Real): Vector = this :* scalar.toDouble
  
  def *: (scalar: Double): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Real = {
    if (dimension != that.dimension)
      throw new DimensionException(Space.toString +" ⋅ "+ that.Space.toString)
    var s = 0.0
    var i = 0
    while (i < dimension) {
      s += this(i) * that(i)
      i += 1
    }
    new Real(s)
  }
}
