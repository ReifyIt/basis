/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import generic._

trait MatrixRMxN extends MatrixFMxN { self =>
  override type Matrix >: self.type <: MatrixRMxN {
    type Matrix       = self.Matrix
    type Transpose    = self.Transpose
    type RowVector    = self.RowVector
    type ColumnVector = self.ColumnVector
  }
  
  override type Transpose <: MatrixRMxN {
    type Matrix       = self.Transpose
    type Transpose    = self.Matrix
    type RowVector    = self.ColumnVector
    type ColumnVector = self.RowVector
  }
  
  override type RowVector <: VectorRN {
    type Vector = self.RowVector
  }
  
  override type ColumnVector <: VectorRN {
    type Vector = self.ColumnVector
  }
  
  override type Scalar = Real
  
  override def Space: RMxN {
    type Matrix       = self.Matrix
    type Transpose    = self.Transpose
    type ColumnVector = self.ColumnVector
    type RowVector    = self.RowVector
  }
  
  def apply(k: Int): Double
  
  def apply(i: Int, j: Int): Double = {
    if (i < 0 || i >= M || j < 0 || j >= N)
      throw new IndexOutOfBoundsException("row "+ i +", "+"column "+ j)
    apply(N * i + j)
  }
  
  def entry(k: Int): Real = new Real(this(k))
  
  override def entry(i: Int, j: Int): Real = new Real(this(i, j))
  
  override def row(i: Int): RowVector = {
    if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
    val coords = new Array[Double](N)
    var j = 0
    var n = N * i
    while (j < N) {
      coords(j) = this(n)
      j += 1
      n += 1
    }
    Space.Row(coords)
  }
  
  override def column(j: Int): ColumnVector = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("column "+ j)
    val coords = new Array[Double](M)
    var i = 0
    var m = j
    while (i < M) {
      coords(i) = this(m)
      i += 1
      m += N
    }
    Space.Column(coords)
  }
  
  override def + (that: Matrix): Matrix = {
    if (M != that.M || N != that.N)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) + that(k)
      k += 1
    }
    Space(entries)
  }
  
  override def unary_- : Matrix = {
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = -this(k)
      k += 1
    }
    Space(entries)
  }
  
  override def - (that: Matrix): Matrix = {
    if (M != that.M || N != that.N)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) - that(k)
      k += 1
    }
    Space(entries)
  }
  
  override def :* (scalar: Real): Matrix = this :* scalar.toDouble
  
  def :* (scalar: Double): Matrix = {
    val entries = new Array[Double](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) * scalar
      k += 1
    }
    Space(entries)
  }
  
  override def *: (scalar: Real): Matrix = this :* scalar.toDouble
  
  def *: (scalar: Double): Matrix = this :* scalar
  
  override def :* (vector: RowVector): ColumnVector = {
    if (N != vector.dimension)
      throw new DimensionException(Space.toString +" :* "+ vector.Space.toString)
    val coords = new Array[Double](M)
    var i = 0
    var i0 = 0
    while (i < M) {
      var x = 0.0
      var n = i0
      var j = 0
      while (j < N) {
        x += this(n) * vector(j)
        n += 1
        j += 1
      }
      coords(i) = x
      i += 1
      i0 += N
    }
    Space.Column(coords)
  }
  
  override def *: (vector: ColumnVector): RowVector = {
    if (vector.dimension != M)
      throw new DimensionException(vector.Space.toString +" *: "+ Space.toString)
    val coords = new Array[Double](N)
    var j = 0
    while (j < N) {
      var x = 0.0
      var n = j
      var i = 0
      while (i < M) {
        x += vector(i) * this(n)
        n += N
        i += 1
      }
      coords(j) = x
      j += 1
    }
    Space.Row(coords)
  }
  
  override def transpose: Transpose = {
    val entries = new Array[Double](N * M)
    var k = 0
    var j = 0
    while (j < N) {
      var n = j
      var i = 0
      while (i < M) {
        entries(k) = this(n)
        n += N
        k += 1
        i += 1
      }
      j += 1
    }
    Space.Transpose(entries)
  }
}
