/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.arithmetic

import basis.algebra._

/** An abstract 2-dimensional vector space over a ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam S   The set of scalars.
  */
trait F2[S <: Ring with Singleton] extends FN[S] {
  trait Element extends Any with super.Element {
    override protected def Vector: F2.this.type = F2.this
    
    /** Returns the 𝑥-coordinate of this $vector. */
    def x: Scalar
    
    /** Returns the 𝑦-coordinate of this $vector. */
    def y: Scalar
    
    override def N: Int = 2
    
    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }
    
    override def + (that: Vector): Vector =
      Vector(x + that.x, y + that.y)
    
    override def unary_- : Vector = Vector(-x, -y)
    
    override def - (that: Vector): Vector =
      Vector(x - that.x, y - that.y)
    
    override def :* (scalar: Scalar): Vector =
      Vector(x * scalar, y * scalar)
    
    override def *: (scalar: Scalar): Vector =
      Vector(scalar * x, scalar * y)
    
    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y
  }
  
  override type Vector <: Element
  
  override def N: Int = 2
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z)
  }
  
  /** Returns a new vector with the given 𝑥 and 𝑦 coordinates. */
  def apply(x: Scalar, y: Scalar): Vector
  
  override def apply(coords: Scalar*): Vector = {
    if (coords.length != 2) throw new DimensionException
    apply(coords(0), coords(1))
  }
  
  /** Extracts the 𝑥 and 𝑦 coordinates from the given vector. */
  def unapply(vector: Vector): Option[(Scalar, Scalar)] =
    Some(vector.x, vector.y)
}

object F2 {
  /** Returns a 2-dimensional vector space over the given ring. */
  def apply(Scalar: Ring): F2[Scalar.type] = new Space[Scalar.type](Scalar)
  
  /** A generic 2-dimensional vector space over a ring.
    * 
    * @tparam S    The set of scalars.
    */
  private final class Space[S <: Ring with Singleton](override val Scalar: S) extends F2[S] {
    final class Element(val x: Scalar, val y: Scalar) extends super.Element
    
    override type Vector = Element
    
    override lazy val zero: Vector = super.zero
    
    override def apply(x: Scalar, y: Scalar): Vector = new Vector(x, y)
    
    override def toString: String = "F2"+"("+ Scalar +")"
  }
}
