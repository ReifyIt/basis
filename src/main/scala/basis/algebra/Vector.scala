/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector extends Any with Linear {
  override type Vector
  
  override type Scalar
  
  def N: Int
  
  def apply(i: Int): Scalar
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Scalar): Vector
  
  override def *: (scalar: Scalar): Vector
  
  def ⋅ (that: Vector): Scalar
}

object Vector {
  trait Space extends Ring.Scalar with Linear.Space { self =>
    override type Vector <: basis.algebra.Vector {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    override def Scalar: Ring.Space {
      type Vector = self.Scalar
    }
    
    def N: Int
    
    def apply(coords: TraversableOnce[Scalar]): Vector
    
    def map[W <: basis.algebra.Vector { type Vector = W; type Scalar = self.Scalar }]
        (that: Vector.Space { type Vector = W; type Scalar = self.Scalar })
      : Matrix.Space { type Row = self.Vector; type Col = W; type Scalar = self.Scalar } =
      new generic.MatrixFMxN.Space(this, that, Scalar)
  }
  
  trait Template extends Any with Equals with Vector { self =>
    override type Vector <: basis.algebra.Vector {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    def Vector: basis.algebra.Vector.Space {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override def N: Int
    
    override def apply(i: Int): Scalar
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (apply(i) + that.apply(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def unary_- : Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (-apply(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (apply(i) - that.apply(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def :* (scalar: Scalar): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (apply(i) * scalar).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def *: (scalar: Scalar): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (scalar * apply(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def ⋅ (that: Vector): Scalar = {
      if (N != that.N || N <= 0) throw new DimensionException
      var s = apply(0) * that.apply(0)
      var i = 1
      while (i < N) {
        s += apply(i) * that.apply(i)
        i += 1
      }
      s
    }
    
    override def canEqual(other: Any): Boolean =
      other.isInstanceOf[basis.algebra.Vector.Template]
    
    override def equals(other: Any): Boolean = other match {
      case that: basis.algebra.Vector.Template =>
        var equal = that.canEqual(this) && N == that.N
        var i = 0
        while (i < N && equal) {
          equal = apply(i).equals(that.apply(i))
          i += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      import basis.util.MurmurHash._
      var h = -1736520349
      var i = 0
      while (i < N) {
        h = mix(h, apply(i))
        i += 1
      }
      mash(h)
    }
    
    override def toString: String = {
      val s = new StringBuilder(Vector.toString)
      s.append('(')
      if (N > 0) {
        s.append(apply(0))
        var i = 1
        while (i < N) {
          s.append(", ").append(apply(i))
          i += 1
        }
      }
      s.append(')')
      s.toString
    }
  }
}
