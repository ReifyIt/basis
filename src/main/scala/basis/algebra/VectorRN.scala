/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorRN[V <: VectorRN[V]] extends VectorFN[V, Real] {
  def Space: RN {
    type Vector = V
  }
  
  def apply(i: Int): Double
  
  def coord(i: Int): Real = new Real(this(i))
  
  override def + (that: V): V = {
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
  
  override def unary_- : V = {
    val coords = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = -this(i)
      i += 1
    }
    Space(coords)
  }
  
  override def - (that: V): V = {
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
  
  override def :* (scalar: Real): V = this :* scalar.toDouble
  
  def :* (scalar: Double): V = {
    val coords = new Array[Double](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = this(i) * scalar
      i += 1
    }
    Space(coords)
  }
  
  override def *: (scalar: Real): V = this :* scalar.toDouble
  
  def *: (scalar: Double): V = this :* scalar
}
