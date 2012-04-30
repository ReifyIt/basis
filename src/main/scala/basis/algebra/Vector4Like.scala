/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector4Like extends Any with VectorLike with Vector4 { self =>
  override type Vector <: Vector4 {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  override def Vector: Vector4Space {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override def x: Scalar
  override def y: Scalar
  override def z: Scalar
  override def w: Scalar
  
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
