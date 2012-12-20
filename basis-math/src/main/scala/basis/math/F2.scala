/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** An abstract 2-dimensional vector space over a ring.
  * 
  * @author Chris Sachs
  */
trait F2 extends FN {
  trait Value extends Any with super.Value {
    /** Returns the 𝑥-coordinate of this $vector. */
    def x: Scalar
    
    /** Returns the 𝑦-coordinate of this $vector. */
    def y: Scalar
    
    override def dim: Int = 2
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case _ => throw new java.lang.IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      F2.this.apply(x + that.x, y + that.y)
    
    override def unary_- : Vector =
      F2.this.apply(-x, -y)
    
    override def - (that: Vector): Vector =
      F2.this.apply(x - that.x, y - that.y)
    
    override def :* (scalar: Scalar): Vector =
      F2.this.apply(x * scalar, y * scalar)
    
    override def *: (scalar: Scalar): Vector =
      F2.this.apply(scalar * x, scalar * y)
  }
  
  override type Vector <: Value
  
  override def dim: Int = 2
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z)
  }
  
  /** Returns a new vector with 𝑥 and 𝑦 coordinates. */
  def apply(x: Scalar, y: Scalar): Vector
  
  override def apply(coords: Array[Scalar]): Vector = {
    if (coords.length != 2) throw new DimensionException
    apply(coords(0), coords(1))
  }
  
  /** Extracts the 𝑥 and 𝑦 coordinates from a vector. */
  def unapply(vector: Vector): Option[(Scalar, Scalar)] =
    Some((vector.x, vector.y))
}
