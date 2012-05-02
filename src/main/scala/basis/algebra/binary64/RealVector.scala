/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RealVector extends Any with Vector {
  override type Vector
  
  override type Scalar = Real
  
  override def N: Int
  
  override def apply(i: Int): Real
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Real): Vector
  
  override def *: (scalar: Real): Vector
  
  override def ⋅ (that: Vector): Real
}

object RealVector {
  trait Space extends RealField.Scalar with Vector.Space { self =>
    override type Vector <: RealVector {
      type Vector = self.Vector
    }
    
    override type Scalar = Real
    
    override def Scalar = Real
    
    override def N: Int
    
    override def apply(coords: TraversableOnce[Real]): Vector
    
    def apply(coords: Array[Double]): Vector
    
    override def map[W <: basis.algebra.Vector { type Vector = W; type Scalar = Real }]
        (that: Vector.Space { type Vector = W; type Scalar = Real })
      : Matrix.Space { type Row = self.Vector; type Col = W; type Scalar = Real } = {
      if (that.isInstanceOf[RealMatrix]) {
        type SomeRealVector[V] = RealVector { type Vector = V }
        this.map(that.asInstanceOf[Space { type Vector <: SomeRealVector[Vector] }]).
          asInstanceOf[Matrix.Space { type Row = self.Vector; type Col = W; type Scalar = Real}]
      }
      else super.map(that)
    }
    
    def map[W <: RealVector { type Vector = W }]
        (that: Space { type Vector = W })
      : RealMatrix.Space { type Row = self.Vector; type Col = W } =
      new MatrixRMxN.Space(this, that)
  }
  
  trait Template extends Any with Vector.Template with RealVector { self =>
    override type Vector <: RealVector {
      type Vector = self.Vector
    }
    
    override def Vector: RealVector.Space {
      type Vector = self.Vector
    }
    
    override def N: Int
    
    override def apply(i: Int): Real
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = apply(i) + that.apply(i)
        i += 1
      }
      Vector(coords)
    }
    
    override def unary_- : Vector = {
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = -apply(i)
        i += 1
      }
      Vector(coords)
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = apply(i) - that.apply(i)
        i += 1
      }
      Vector(coords)
    }
    
    override def :* (scalar: Real): Vector = {
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = apply(i) * scalar
        i += 1
      }
      Vector(coords)
    }
    
    override def *: (scalar: Real): Vector = this :* scalar
    
    override def ⋅ (that: Vector): Real = {
      if (N != that.N || N <= 0) throw new DimensionException
      var s = 0.0
      var i = 0
      while (i < N) {
        s += apply(i) * that.apply(i)
        i += 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: RealVector.Template =>
        var equal = that.canEqual(this) && N == that.N
        var i = 0
        while (i < N && equal) {
          equal = apply(i) == that.apply(i)
          i += 1
        }
        equal
      case _ => super.equals(other)
    }
    
    override def hashCode: Int = {
      import basis.util.MurmurHash._
      var h = -1736520349
      var i = 0
      while (i < N) {
        h = mix(h, apply(i).hashCode)
        i += 1
      }
      mash(h)
    }
  }
}
