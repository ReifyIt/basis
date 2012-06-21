/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector2Space[S <: Ring with Singleton] extends VectorSpace[S] {
  trait Element extends Any with super.Element {
    override protected def Vector: Vector2Space.this.type = Vector2Space.this
    
    def x: Scalar
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
  
  override def apply(coords: Scalar*): Vector = {
    if (coords.length != 2) throw new DimensionException
    apply(coords(0), coords(1))
  }
  
  def apply(x: Scalar, y: Scalar): Vector
  
  def unapply(vector: Vector): Option[(Scalar, Scalar)] =
    Some(vector.x, vector.y)
  
  override def zero: Vector = {
    val z = Scalar.zero
    apply(z, z)
  }
}
