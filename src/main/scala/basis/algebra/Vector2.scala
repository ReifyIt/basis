/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector2 extends Any with Vector {
  override type Vector
  
  override type Scalar
  
  def x: Scalar
  def y: Scalar
  
  override def N: Int
  
  override def apply(i: Int): Scalar
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Scalar): Vector
  
  override def *: (scalar: Scalar): Vector
  
  override def ⋅ (that: Vector): Scalar
}

object Vector2 {
  trait Space extends Vector.Space { self =>
    override type Vector <: Vector2 {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    override def N: Int = 2
    
    override def apply(coords: TraversableOnce[Scalar]): Vector = {
      val xs = coords.toSeq
      if (xs.length != 2) throw new DimensionException
      apply(xs(0), xs(1))
    }
    
    def apply(x: Scalar, y: Scalar): Vector
    
    def unapply(vector: Vector): Option[(Scalar, Scalar)] =
      Some(vector.x, vector.y)
  }
  
  trait Template extends Any with Vector.Template with Vector2 { self =>
    override type Vector <: Vector2 {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    override def Vector: Vector2.Space {
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
}
