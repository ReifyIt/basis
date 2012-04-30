/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector2Like extends Any with VectorLike with Vector2 { self =>
  override type Vector <: Vector2 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  override def Vector: Vector2Space {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override def x: Scalar
  override def y: Scalar
  
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
