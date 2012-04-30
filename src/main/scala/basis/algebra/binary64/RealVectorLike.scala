/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import basis.util.MurmurHash._

trait RealVectorLike extends Any with VectorLike with RealVector { self =>
  override type Vector <: RealVector {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override def Vector: RealVectorSpace {
    type Vector = self.Vector
    type Scalar = self.Scalar
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
    case that: RealVectorLike =>
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
    var h = 2011662234
    var i = 0
    while (i < N) {
      h = mix(h, apply(i).hashCode)
      i += 1
    }
    mash(h)
  }
}
