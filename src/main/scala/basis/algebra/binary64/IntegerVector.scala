/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait IntegerVector extends Any with Vector {
  override type Vector
  
  override type Scalar = Integer
  
  override def N: Int
  
  override def apply(i: Int): Integer
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Integer): Vector
  
  override def *: (scalar: Integer): Vector
  
  override def ⋅ (that: Vector): Integer
}

object IntegerVector {
  trait Space extends OrderedRing.Scalar with Vector.Space { self =>
    override type Vector <: IntegerVector {
      type Vector = self.Vector
    }
    
    override type Scalar = Integer
    
    override def Scalar = Integer
    
    override def N: Int
    
    override def apply(coords: TraversableOnce[Integer]): Vector
    
    def apply(coords: Array[Long]): Vector
  }
  
  trait Template extends Any with Vector.Template with IntegerVector { self =>
    override type Vector <: IntegerVector {
      type Vector = self.Vector
    }
    
    override def Vector: IntegerVector.Space {
      type Vector = self.Vector
    }
    
    override def N: Int
    
    override def apply(i: Int): Integer
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = apply(i) + that.apply(i)
        i += 1
      }
      Vector(coords)
    }
    
    override def unary_- : Vector = {
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = -apply(i)
        i += 1
      }
      Vector(coords)
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = apply(i) - that.apply(i)
        i += 1
      }
      Vector(coords)
    }
    
    override def :* (scalar: Integer): Vector = {
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = apply(i) * scalar
        i += 1
      }
      Vector(coords)
    }
    
    override def *: (scalar: Integer): Vector = this :* scalar
    
    override def ⋅ (that: Vector): Integer = {
      if (N != that.N || N <= 0) throw new DimensionException
      var s = 0L
      var i = 0
      while (i < N) {
        s += apply(i) * that.apply(i)
        i += 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: IntegerVector.Template =>
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
