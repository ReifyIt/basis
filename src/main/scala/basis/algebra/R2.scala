/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait R2 extends F2 with RN { self =>
  override type Vector <: VectorR2 {
    type Vector = self.Vector
  }
  
  override def zero: Vector = apply(0.0, 0.0)
  
  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 2) throw new DimensionException
    apply(coords(0), coords(1))
  }
  
  override def apply(x: Scalar, y: Scalar): Vector =
    apply(x.toDouble, y.toDouble)
  
  def apply(x: Double, y: Double): Vector
}

object R2 extends HilbertSpace with R2 {
  final class Vector(val x: Double, val y: Double) extends VectorR2 {
    override type Vector = R2.Vector
    
    override def Space = R2
    
    def apply(i: Int): Double = i match {
      case 0 => x
      case 1 => y
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
  }
  
  override val zero: Vector = super.zero
  
  def apply(x: Double, y: Double): Vector =
    new Vector(x, y)
  
  def unapply(vector: Vector): Some[(Double, Double)] =
    Some(vector.x, vector.y)
  
  def innerProduct(u: Vector, v: Vector): Real = u ⋅ v
  
  override def toString: String = "R2"
}
