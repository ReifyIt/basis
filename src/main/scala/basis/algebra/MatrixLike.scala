/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

trait MatrixLike extends Any with Equals with Matrix { self =>
  override type Matrix <: basis.algebra.Matrix {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
    type Scalar = self.Scalar
  }
  
  override type T <: basis.algebra.Matrix {
    type Matrix = self.T
    type T      = self.Matrix
    type Row    = self.Col
    type Col    = self.Row
    type Scalar = self.Scalar
  }
  
  override type Row <: basis.algebra.Vector {
    type Vector = self.Row
    type Scalar = self.Scalar
  }
  
  override type Col <: basis.algebra.Vector {
    type Vector = self.Col
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  def Matrix: MatrixSpace {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
    type Scalar = self.Scalar
  }
  
  override def Row: VectorSpace {
    type Vector = self.Row
    type Scalar = self.Scalar
  }
  
  override def Col: VectorSpace {
    type Vector = self.Col
    type Scalar = self.Scalar
  }
  
  override def M: Int = Col.N
  
  override def N: Int = Row.N
  
  override def apply(k: Int): Scalar
  
  override def apply(i: Int, j: Int): Scalar = {
    if (i < 0 || i >= M || j < 0 || j >= N)
      throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
    apply(N * i + j)
  }
  
  override def row(i: Int): Row = {
    if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
    val coords = new Array[AnyRef](N)
    var j = 0
    var n = N * i
    while (j < coords.length) {
      coords(j) = apply(n).asInstanceOf[AnyRef]
      j += 1
      n += 1
    }
    Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  override def col(j: Int): Col = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("col "+ j)
    val coords = new Array[AnyRef](M)
    var i = 0
    var m = j
    while (i < coords.length) {
      coords(i) = apply(m).asInstanceOf[AnyRef]
      i += 1
      m += N
    }
    Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  override def + (that: Matrix): Matrix = {
    if (M != that.M || N != that.N) throw new DimensionException
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = (apply(k) + that.apply(k)).asInstanceOf[AnyRef]
      k += 1
    }
    Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def unary_- : Matrix = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = (-apply(k)).asInstanceOf[AnyRef]
      k += 1
    }
    Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def - (that: Matrix): Matrix = {
    if (M != that.M || N != that.N) throw new DimensionException
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = (apply(k) - that.apply(k)).asInstanceOf[AnyRef]
      k += 1
    }
    Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def :* (scalar: Scalar): Matrix = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = (apply(k) * scalar).asInstanceOf[AnyRef]
      k += 1
    }
    Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def *: (scalar: Scalar): Matrix = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = (scalar * apply(k)).asInstanceOf[AnyRef]
      k += 1
    }
    Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def :⋅ (vector: Row): Col = {
    if (N != vector.N || N <= 0) throw new DimensionException
    val coords = new Array[AnyRef](M)
    var i = 0
    var i0 = 0
    while (i < coords.length) {
      var s = apply(i0) * vector.apply(0)
      var n = i0 + 1
      var j = 1
      while (j < N) {
        s += apply(n) * vector.apply(j)
        n += 1
        j += 1
      }
      coords(i) = s.asInstanceOf[AnyRef]
      i += 1
      i0 += N
    }
    Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  override def ⋅: (vector: Col): Row = {
    if (vector.N != M || M <= 0) throw new DimensionException
    val coords = new Array[AnyRef](N)
    var j = 0
    while (j < coords.length) {
      var s = vector.apply(0) * apply(j)
      var n = j + N
      var i = 1
      while (i < M) {
        s += vector.apply(i) * apply(n)
        n += N
        i += 1
      }
      coords(j) = s.asInstanceOf[AnyRef]
      j += 1
    }
    Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  override def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = self.Scalar }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = self.Scalar })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = self.Scalar } = {
    if (N != that.M || N <= 0) throw new DimensionException
    val P = that.N
    val entries = new Array[AnyRef](M * P)
    var k = 0
    var i = 0
    var i0 = 0
    while (i < M) {
      var j = 0
      while (j < P) {
        var s = apply(i0) * that.apply(j)
        var m = i0 + 1
        var n = j + P
        var d = 1
        while (d < N) {
          s += apply(m) * that.apply(n)
          m += 1
          n += P
          d += 1
        }
        entries(k) = s.asInstanceOf[AnyRef]
        k += 1
        j += 1
      }
      i += 1
      i0 += N
    }
    (that.Row map Col).apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def T: T = {
    val entries = new Array[AnyRef](N * M)
    var k = 0
    var j = 0
    while (j < N) {
      var n = j
      var i = 0
      while (i < M) {
        entries(k) = apply(n).asInstanceOf[AnyRef]
        n += N
        k += 1
        i += 1
      }
      j += 1
    }
    Matrix.T(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[MatrixLike]
  
  override def equals(other: Any): Boolean = other match {
    case that: MatrixLike =>
      var dim = M * N
      var equal = that.canEqual(this) && M == that.M && N == that.N
      var k = 0
      while (k < dim && equal) {
        equal = apply(k).equals(that.apply(k))
        k += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    var dim = M * N
    var h = 41930520
    var k = 0
    while (k < dim) {
      h = mix(h, apply(k))
      k += 1
    }
    mash(h)
  }
  
  override def toString: String = {
    val s = new StringBuilder(Matrix.toString)
    s.append('(')
    if (M > 0 && N > 0) {
      s.append(apply(0))
      var k = 1
      var j = 1
      var i = 0
      while (i < M) {
        while (j < N) {
          (if (j != 0) s.append(", ") else s.append(",  ")).append(apply(k))
          k += 1
          j += 1
        }
        j = 0
        i += 1
      }
    }
    s.append(')')
    s.toString
  }
}
