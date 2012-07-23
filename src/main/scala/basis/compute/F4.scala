/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.compute

import basis.algebra._

/** An abstract 4-dimensional vector space over a ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam S   The set of scalars.
  */
trait F4[S <: Ring with Singleton] extends FN[S] {
  trait Element extends Any with super.Element {
    override protected def Vector: F4.this.type = F4.this
    
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
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z, z, z)
  }
  
  /** Returns a new vector with the given 𝑥, 𝑦, 𝑧 and 𝑤 coordinates. */
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector
  
  override def apply(coords: Scalar*): Vector = {
    if (coords.length != 4) throw new DimensionException
    apply(coords(0), coords(1), coords(2), coords(3))
  }
  
  /** Extracts the 𝑥, 𝑦, 𝑧 and 𝑤 coordinates from the given vector. */
  def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z, vector.w)
}

object F4 {
  /** Returns a 4-dimensional vector space over the given ring. */
  def apply(Scalar: Ring): F4[Scalar.type] = new Space[Scalar.type](Scalar)
  
  /** A generic 4-dimensional vector space over a ring.
    * 
    * @tparam S    The set of scalars.
    */
  final class Space[S <: Ring with Singleton](override val Scalar: S) extends F4[S] {
    final class Element(val x: Scalar, val y: Scalar, val z: Scalar, val w: Scalar) extends super.Element
    
    override type Vector = Element
    
    override lazy val zero: Vector = super.zero
    
    override def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector = new Vector(x, y, z, w)
    
    override def toString: String = "F4"+"("+ Scalar +")"
  }
}
