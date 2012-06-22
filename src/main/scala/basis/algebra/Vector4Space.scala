/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract 4-dimensional coordinate space over a commutative ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam S   The singleton type of the scalar structure of this $Structure.
  * 
  * @define Structure   `Vector4Space`
  * @define vector      vector
  * @define scalar      scalar
  */
trait Vector4Space[S <: Ring with Singleton] extends VectorSpace[S] {
  /** A vector element of this $Structure. */
  trait Element extends Any with super.Element {
    override protected def Vector: Vector4Space.this.type = Vector4Space.this
    
    /** Returns the 𝑥-coordinate of this $vector. */
    def x: Scalar
    
    /** Returns the 𝑦-coordinate of this $vector. */
    def y: Scalar
    
    /** Returns the 𝑧-coordinate of this $vector. */
    def z: Scalar
    
    /** Returns the 𝑤-coordinate of this $vector. */
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
  
  override def apply(coords: Scalar*): Vector = {
    if (coords.length != 4) throw new DimensionException
    apply(coords(0), coords(1), coords(2), coords(3))
  }
  
  /** Returns a new $vector with the given 𝑥, 𝑦, 𝑧 and 𝑤 coordinates. */
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector
  
  /** Extracts the 𝑥, 𝑦, 𝑧 and 𝑤 coordinates from the given $vector. */
  def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z, z)
  }
}
