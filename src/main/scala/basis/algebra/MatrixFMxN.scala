/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

trait MatrixFMxN extends Equals with LinearVector { self =>
  type Matrix >: self.type <: MatrixFMxN {
    type Matrix       = self.Matrix
    type Transpose    = self.Transpose
    type RowVector    = self.RowVector
    type ColumnVector = self.ColumnVector
    type Scalar       = self.Scalar
  }
  
  type Transpose <: MatrixFMxN {
    type Matrix       = self.Transpose
    type Transpose    = self.Matrix
    type RowVector    = self.ColumnVector
    type ColumnVector = self.RowVector
    type Scalar       = self.Scalar
  }
  
  type RowVector <: VectorFN {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  type ColumnVector <: VectorFN {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  override type Point = Matrix
  
  override type Vector = Matrix
  
  override def Space: FMxN {
    type Matrix       = self.Matrix
    type Transpose    = self.Transpose
    type ColumnVector = self.ColumnVector
    type RowVector    = self.RowVector
    type Scalar       = self.Scalar
  }
  
  def entry(k: Int): Scalar
  
  def entry(i: Int, j: Int): Scalar = {
    if (i < 0 || i >= M || j < 0 || j >= N)
      throw new IndexOutOfBoundsException("row "+ i +", "+"column "+ j)
    entry(N * i + j)
  }
  
  def M: Int = Space.Column.dimension
  
  def N: Int = Space.Row.dimension
  
  def row(i: Int): RowVector = {
    if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
    val coords = new Array[AnyRef](N)
    var j = 0
    var n = N * i
    while (j < N) {
      coords(j) = entry(n)
      j += 1
      n += 1
    }
    Space.Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
 def column(j: Int): ColumnVector = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("column "+ j)
    val coords = new Array[AnyRef](M)
    var i = 0
    var m = j
    while (i < M) {
      coords(i) = entry(m)
      i += 1
      m += N
    }
    Space.Column(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def + (that: Matrix): Matrix = {
    if (M != that.M || N != that.N)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = entry(k) + that.entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def unary_- : Matrix = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = -entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def - (that: Matrix): Matrix = {
    if (M != that.M || N != that.N)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = entry(k) - that.entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def :* (scalar: Scalar): Matrix = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = entry(k) * scalar
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def *: (scalar: Scalar): Matrix = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = scalar * entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def :* (vector: RowVector): ColumnVector = {
    if (N != vector.dimension)
      throw new DimensionException(Space.toString +" :* "+ vector.Space.toString)
    val coords = new Array[AnyRef](M)
    var i = 0
    var i0 = 0
    while (i < M) {
      var x = Space.Scalar.zero
      var n = i0
      var j = 0
      while (j < N) {
        x += entry(n) * vector.coord(j)
        n += 1
        j += 1
      }
      coords(i) = x
      i += 1
      i0 += N
    }
    Space.Column(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def *: (vector: ColumnVector): RowVector = {
    if (vector.dimension != M)
      throw new DimensionException(vector.Space.toString +" *: "+ Space.toString)
    val coords = new Array[AnyRef](N)
    var j = 0
    while (j < N) {
      var x = Space.Scalar.zero
      var n = j
      var i = 0
      while (i < M) {
        x += vector.coord(i) * entry(n)
        n += N
        i += 1
      }
      coords(j) = x
      j += 1
    }
    Space.Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def transpose: Transpose = {
    val entries = new Array[AnyRef](N * M)
    var k = 0
    var j = 0
    while (j < N) {
      var n = j
      var i = 0
      while (i < M) {
        entries(k) = entry(n)
        n += N
        k += 1
        i += 1
      }
      j += 1
    }
    Space.Transpose(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[MatrixFMxN]
  
  override def equals(other: Any): Boolean = other match {
    case that: MatrixFMxN =>
      val size = M * N
      var equal = that.canEqual(this) && M == that.M && N == that.N
      var k = 0
      while (k < size && equal) {
        equal = entry(k).equals(that.entry(k))
        k += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    val size = M * N
    var h = 1809920179
    var k = 0
    while (k < size) {
      mix(h, entry(k))
      k += 1
    }
    mash(h)
  }
  
  override def toString: String = {
    val s = new StringBuilder(Space.toString)
    s.append('(')
    if (0 < M * N) s.append(entry(0))
    var k = 1
    var j = 1
    var i = 0
    while (i < M) {
      while (j < N) {
        if (j != 0) s.append(", ") else s.append(",  ")
        s.append(entry(k))
        k += 1
        j += 1
      }
      j = 0
      i += 1
    }
    s.append(')')
    s.toString
  }
}
