/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector4Space[S <: Ring with Singleton] extends VectorSpace[S] {
  trait Element extends Any with super.Element {
    override protected def Vector: Vector4Space.this.type = Vector4Space.this
    
    def x: Scalar
    def y: Scalar
    def z: Scalar
    def w: Scalar
    
    override def N: Int = 4
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      Vector(x + that.x, y + that.y, z + that.z, w + that.w)
    
    override def unary_- : Vector = Vector(-x, -y, -z, -w)
    
    override def - (that: Vector): Vector =
      Vector(x - that.x, y - that.y, z - that.z, w - that.w)
    
    override def :* (scalar: Scalar): Vector =
      Vector(x * scalar, y * scalar, z * scalar, w * scalar)
    
    override def *: (scalar: Scalar): Vector =
      Vector(scalar * x, scalar * y, scalar * z, scalar * w)
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y + z * that.z + w * that.w
  }
  
  override type Vector <: Element
  
  override def N: Int = 4
  
  override def apply(coords: TraversableOnce[Scalar]): Vector = {
    val xs = coords.toSeq
    if (xs.length != 4) throw new DimensionException
    apply(xs(0), xs(1), xs(2), xs(3))
  }
  
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector
  
  def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z, z)
  }
}
