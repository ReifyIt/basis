/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait R4 extends F4 with RealCoordinateSpace {
  type Vector <: VectorR4[Vector]
  
  override def zero: Vector = apply(0.0, 0.0, 0.0, 0.0)
  
  def apply(coords: Array[Double]): Vector = {
    if (coords.length != 4) throw new DimensionException
    apply(coords(0), coords(1), coords(2), coords(3))
  }
  
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector =
    apply(x.toDouble, y.toDouble, z.toDouble, w.toDouble)
  
  def apply(x: Double, y: Double, z: Double, w: Double): Vector
}

object R4 extends HilbertSpace with R4 {
  final class Vector(val x: Double, val y: Double, val z: Double, val w: Double)
    extends VectorR4[Vector] {
    
    def Space = R4
    
    def apply(i: Int): Double = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    def ⋅ (that: Vector): Double = x * that.x + y * that.y + z * that.z + w * that.w
    
    def norm: Double = math.sqrt(x * x + y * y + z * z + w * w)
  }
  
  override val zero: Vector = super.zero
  
  def apply(x: Double, y: Double, z: Double, w: Double): Vector =
    new Vector(x, y, z, w)
  
  def unapply(vector: Vector): Some[(Double, Double, Double, Double)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  def innerProduct(u: Vector, v: Vector): Real = new Real(u ⋅ v)
  
  override def norm(u: Vector): Scalar = new Scalar(u.norm)
  
  override def normalize(u: Vector): Vector = u :* (1.0 / u.norm)
  
  override def distance(u: Vector, v: Vector): Scalar = {
    val dx = u.x - v.x
    val dy = u.y - v.y
    val dz = u.z - v.z
    val dw = u.w - v.w
    new Scalar(math.sqrt(dx * dx + dy * dy + dz * dz + dw * dw))
  }
  
  override def toString: String = "R4"
}
