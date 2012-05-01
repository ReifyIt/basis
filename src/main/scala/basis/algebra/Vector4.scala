/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector4 extends Any with Vector {
  override type Vector
  
  override type Scalar
  
  def x: Scalar
  def y: Scalar
  def z: Scalar
  def w: Scalar
  
  override def N: Int
  
  override def apply(i: Int): Scalar
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Scalar): Vector
  
  override def *: (scalar: Scalar): Vector
  
  def ⋅ (that: Vector): Scalar
}

object Vector4 {
  trait Space extends Vector.Space { self =>
    override type Vector <: Vector4 {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    override def N: Int = 4
    
    override def apply(coords: TraversableOnce[Scalar]): Vector = {
      val xs = coords.toSeq
      if (xs.length != 4) throw new DimensionException
      apply(xs(0), xs(1), xs(2), xs(3))
    }
    
    def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector
    
    def unapply(vector: Vector): Option[(Scalar, Scalar, Scalar, Scalar)] =
      Some(vector.x, vector.y, vector.z, vector.w)
  }
  
  trait Template extends Any with Vector.Template with Vector4 { self =>
    override type Vector <: Vector4 {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    override def Vector: Vector4.Space {
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
}
