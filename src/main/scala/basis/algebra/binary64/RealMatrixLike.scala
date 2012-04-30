/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import basis.util.MurmurHash._

trait RealMatrixLike extends Any with MatrixLike with RealMatrix { self =>
  override type Matrix <: RealMatrix {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
  }
  
  override type T <: RealMatrix {
    type Matrix = self.T
    type T      = self.Matrix
    type Row    = self.Col
    type Col    = self.Row
  }
  
  override type Row <: RealVector {
    type Vector = self.Row
  }
  
  override type Col <: RealVector {
    type Vector = self.Col
  }
  
  override def Matrix: RealMatrixSpace {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
  }
  
  override def Row: RealVectorSpace {
    type Vector = self.Row
  }
  
  override def Col: RealVectorSpace {
    type Vector = self.Col
  }
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Real
  
  override def apply(i: Int, j: Int): Real = {
    if (i < 0 || i >= M || j < 0 || j >= N)
      throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
    apply(N * i + j)
  }
  
  override def row(i: Int): Row = {
    if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
    val coords = new Array[Double](N)
    var j = 0
    var n = N * i
    while (j < coords.length) {
      coords(j) = apply(n)
      j += 1
      n += 1
    }
    Row(coords)
  }
  
  override def col(j: Int): Col = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("col "+ j)
    val coords = new Array[Double](M)
    var i = 0
    var m = j
    while (i < coords.length) {
      coords(i) = apply(m)
      i += 1
      m += N
    }
    Col(coords)
  }
  
  override def + (that: Matrix): Matrix = {
    if (M != that.M || N != that.N) throw new DimensionException
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = apply(k) + that.apply(k)
      k += 1
    }
    Matrix(entries)
  }
  
  override def unary_- : Matrix = {
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = -apply(k)
      k += 1
    }
    Matrix(entries)
  }
  
  override def - (that: Matrix): Matrix = {
    if (M != that.M || N != that.N) throw new DimensionException
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = apply(k) - that.apply(k)
      k += 1
    }
    Matrix(entries)
  }
  
  override def :* (scalar: Real): Matrix = {
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = apply(k) * scalar
      k += 1
    }
    Matrix(entries)
  }
  
  override def *: (scalar: Real): Matrix = this :* scalar
  
  override def :⋅ (vector: Row): Col = {
    if (N != vector.N || N <= 0) throw new DimensionException
    val coords = new Array[Double](M)
    var i = 0
    var i0 = 0
    while (i < coords.length) {
      var s = 0.0
      var n = i0
      var j = 0
      while (j < N) {
        s += apply(n) * vector.apply(j)
        n += 1
        j += 1
      }
      coords(i) = s
      i += 1
      i0 += N
    }
    Col(coords)
  }
  
  override def ⋅: (vector: Col): Row = {
    if (vector.N != M || M <= 0) throw new DimensionException
    val coords = new Array[Double](N)
    var j = 0
    while (j < coords.length) {
      var s = 0.0
      var n = j
      var i = 0
      while (i < M) {
        s += vector.apply(i) * apply(n)
        n += N
        i += 1
      }
      coords(j) = s
      j += 1
    }
    Row(coords)
  }
  
  override def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = Real }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = Real })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = Real } = {
    if (that.isInstanceOf[RealMatrix])
      this.⋅(that.asInstanceOf[RealMatrix { type Row <: RealVector { type Vector = Row }; type Col = self.Row }]).
        asInstanceOf[basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = Real }]
    else super.⋅(that)
  }
  
  override def ⋅ [U <: RealVector { type Vector = U }]
      (that: RealMatrix { type Row = U; type Col = self.Row })
    : RealMatrix { type Row = U; type Col = self.Col } = {
    if (N != that.M || N <= 0) throw new DimensionException
    val P = that.N
    val entries = new Array[Double](M * P)
    var k = 0
    var i = 0
    var i0 = 0
    while (i < M) {
      var j = 0
      while (j < P) {
        var s = 0.0
        var m = i0
        var n = j
        var d = 1
        while (d < N) {
          s += apply(m) * that.apply(n)
          m += 1
          n += P
          d += 1
        }
        entries(k) = s
        k += 1
        j += 1
      }
      i += 1
      i0 += N
    }
    (that.Row map Col).apply(entries)
  }
  
  override def T: T = {
    val entries = new Array[Double](N * M)
    var k = 0
    var j = 0
    while (j < N) {
      var n = j
      var i = 0
      while (i < M) {
        entries(k) = apply(n)
        n += N
        k += 1
        i += 1
      }
      j += 1
    }
    Matrix.T(entries)
  }
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[RealMatrixLike]
  
  override def equals(other: Any): Boolean = other match {
    case that: RealMatrixLike =>
      var dim = M * N
      var equal = that.canEqual(this) && M == that.M && N == that.N
      var k = 0
      while (k < dim && equal) {
        equal = apply(k) == that.apply(k)
        k += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    var dim = M * N
    var h = 805766998
    var k = 0
    while (k < dim) {
      h = mix(h, apply(k).hashCode)
      k += 1
    }
    mash(h)
  }
}
