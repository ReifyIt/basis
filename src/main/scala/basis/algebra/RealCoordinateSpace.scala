/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait RealCoordinateSpace extends CoordinateSpace {
  type Vector <: RealCoordinateVector[Vector]
  
  type Scalar = Real
  
  val Scalar = Real
  
  override def zero: Vector = apply(new Array[Double](dimension))
  
  override def apply(coords: Seq[Scalar]): Vector = {
    if (coords.length != dimension) throw new DimensionException
    val xs = new Array[Double](dimension)
    var i = 0
    while (i < xs.length) {
      xs(i) = coords(i).toDouble
      i += 1
    }
    apply(xs)
  }
  
  def apply(coords: Array[Double]): Vector
}
