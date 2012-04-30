/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

trait VectorLike extends Any with Equals with Vector { self =>
  override type Vector <: basis.algebra.Vector {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  def Vector: VectorSpace {
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
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[VectorLike]
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorLike =>
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
    var h = 2011662234
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
