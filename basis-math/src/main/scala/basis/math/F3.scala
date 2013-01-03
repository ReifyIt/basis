/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** An abstract 3-dimensional vector space over a ring.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    VectorSpaces
  */
trait F3 extends FN {
  trait Value extends Any with super.Value {
    /** Returns the 𝑥-coordinate of this $vector. */
    def x: Scalar
    
    /** Returns the 𝑦-coordinate of this $vector. */
    def y: Scalar
    
    /** Returns the 𝑧-coordinate of this $vector. */
    def z: Scalar
    
    override def dim: Int = 3
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new java.lang.IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      F3.this.apply(x + that.x, y + that.y, z + that.z)
    
    override def unary_- : Vector =
      F3.this.apply(-x, -y, -z)
    
    override def - (that: Vector): Vector =
      F3.this.apply(x - that.x, y - that.y, z - that.z)
    
    override def :* (scalar: Scalar): Vector =
      F3.this.apply(x * scalar, y * scalar, z * scalar)
    
    override def *: (scalar: Scalar): Vector =
      F3.this.apply(scalar * x, scalar * y, scalar * z)
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y + z * that.z
    
    /** Returns the cross product of this $vector and another $vector.
      * The name of this method contains the unicode cross product operator (U+2A2F). */
    def ⨯ (that: Vector): Vector =
      F3.this.apply(y * that.z + z * that.y,
                    z * that.x + x * that.z,
                    x * that.y + y * that.x)
  }
  
  override type Vector <: Value
  
  override def dim: Int = 3
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z)
  }
  
  /** Returns a new vector with 𝑥, 𝑦 and 𝑧 coordinates. */
  def apply(x: Scalar, y: Scalar, z: Scalar): Vector
  
  override def apply(coords: Array[Scalar]): Vector = {
    if (coords.length != 3) throw new DimensionException
    apply(coords(0), coords(1), coords(2))
  }
  
  /** Extracts the 𝑥, 𝑦 and 𝑧 coordinates from a vector. */
  def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar)] =
    Some((vector.x, vector.y, vector.z))
}
