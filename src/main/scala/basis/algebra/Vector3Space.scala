/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract space of 3-dimensional vectors over a ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam S   The set of scalars.
  */
trait Vector3Space[S <: Ring with Singleton] extends VectorSpace[S] {
  trait Element extends Any with super.Element {
    override protected def Vector: Vector3Space.this.type = Vector3Space.this
    
    /** Returns the 𝑥-coordinate of this $vector. */
    def x: Scalar
    
    /** Returns the 𝑦-coordinate of this $vector. */
    def y: Scalar
    
    /** Returns the 𝑧-coordinate of this $vector. */
    def z: Scalar
    
    override def N: Int = 3
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      Vector(x + that.x, y + that.y, z + that.z)
    
    override def unary_- : Vector = Vector(-x, -y, -z)
    
    override def - (that: Vector): Vector =
      Vector(x - that.x, y - that.y, z - that.z)
    
    override def :* (scalar: Scalar): Vector =
      Vector(x * scalar, y * scalar, z * scalar)
    
    override def *: (scalar: Scalar): Vector =
      Vector(scalar * x, scalar * y, scalar * z)
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y + z * that.z
    
    /** Returns the cross product of this $vector and another $vector.
      * The name of this method contains the unicode cross product operator (U+2A2F). */
    def ⨯ (that: Vector): Vector =
      Vector(y * that.z + z * that.y,
             z * that.x + x * that.z,
             x * that.y + y * that.x)
  }
  
  override type Vector <: Element
  
  override def N: Int = 3
  
  override def apply(coords: Scalar*): Vector = {
    if (coords.length != 3) throw new DimensionException
    apply(coords(0), coords(1), coords(2))
  }
  
  /** Returns a new vector with the given 𝑥, 𝑦 and 𝑧 coordinates. */
  def apply(x: Scalar, y: Scalar, z: Scalar): Vector
  
  /** Extracts the 𝑥, 𝑦 and 𝑧 coordinates from the given vector. */
  def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z)
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z)
  }
}
