/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

/** An ''M''x''N'' matrix of `Real` values.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a matrix with a row-major entry array.
  * @param  M         The number of rows.
  * @param  N         The number of columns.
  * @param  entries   the array of row-major entries.
  * 
  * @define scalar  `Real` value
  */
final class MatrixRMxN(val M: Int, val N: Int, protected val entries: Array[Double])
  extends RealVector[MatrixRMxN]
    with Matrix[MatrixRMxN, MatrixRMxN, VectorRN, VectorRN, Real] {
  
  /** Constructs a matrix with repeated row-major entry parameters.
    * 
    * @param  M         The number of rows.
    * @param  N         The number of columns.
    * @param  entries   the sequence of row-major entries.
    */
  def this(M: Int, N: Int)(entries: Double*) = this(M, N, entries.toArray[Double])
  
  /** Returns the entry at row ''i'', column ''j''.
    * 
    * @param  i   the zero-based row index.
    * @param  j   the zero-based column index.
    * @return the matrix entry.
    */
  def apply(i: Int, j: Int): Double = {
    if (i < 0 || i >= M || j < 0 || j >= N) throw new IndexOutOfBoundsException("row "+ i +", "+"column "+ j)
    entries(N * i + j)
  }
  
  /** Returns the entry at row `k / N`, column `k % M`.
    * 
    * @param  k   the zero-based row-major index.
    * @return the matrix entry.
    */
  def apply(k: Int): Double = entries(k)
  
  /** Returns the ''j''th column.
    * 
    * @param  j   the zero-based column index.
    * @return the matrix column vector.
    */
  def column(j: Int): VectorRN = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("column "+ j)
    val coordinates = new Array[Double](M)
    var i = 0
    var m = j
    while (i < M) {
      coordinates(i) = this(m)
      i += 1
      m += N
    }
    new VectorRN(coordinates)
  }
  
  /** Returns the ''i''th row.
    * 
    * @param  i   the zero-based row index.
    * @return the matrix row vector.
    */
  def row(i: Int): VectorRN = {
    if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
    val coordinates = new Array[Double](N)
    var j = 0
    var n = N * i
    while (j < N) {
      coordinates(j) = this(n)
      j += 1
      n += 1
    }
    new VectorRN(coordinates)
  }
  
  def + (that: MatrixRMxN): MatrixRMxN = {
    if (M != that.M || N != that.N) throw new DimensionException
    val entries = new Array[Double](this.entries.length)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) + that(k)
      k += 1
    }
    new MatrixRMxN(M, N, entries)
  }
  
  def unary_- : MatrixRMxN = {
    val entries = new Array[Double](this.entries.length)
    var k = 0
    while (k < entries.length) {
      entries(k) = -this(k)
      k += 1
    }
    new MatrixRMxN(M, N, entries)
  }
  
  def - (that: MatrixRMxN): MatrixRMxN = {
    if (M != that.M || N != that.N) throw new DimensionException
    val entries = new Array[Double](this.entries.length)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) - that(k)
      k += 1
    }
    new MatrixRMxN(M, N, entries)
  }
  
  def :* (scalar: Double): MatrixRMxN = {
    val entries = new Array[Double](this.entries.length)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) * scalar
      k += 1
    }
    new MatrixRMxN(M, N, entries)
  }
  
  def *: (scalar: Double): MatrixRMxN =
    this :* scalar
  
  def / (scalar: Double): MatrixRMxN = {
    val entries = new Array[Double](this.entries.length)
    var k = 0
    while (k < entries.length) {
      entries(k) = this(k) / scalar
      k += 1
    }
    new MatrixRMxN(M, N, entries)
  }
  
  def :⋅ (column: VectorRN): VectorRN = {
    if (N != column.dimension) throw new DimensionException
    val coordinates = new Array[Double](M)
    var i = 0
    var i0 = 0
    while (i < M) {
      var dp = 0.0
      var n = i0
      var j = 0
      while (j < N) {
        dp = this(n) * column(j)
        n += 1
        j += 1
      }
      coordinates(i) = dp
      i += 1
      i0 += N
    }
    new VectorRN(coordinates)
  }
  
  def ⋅: (row: VectorRN): VectorRN = {
    if (row.dimension != M) throw new DimensionException
    val coordinates = new Array[Double](N)
    var j = 0
    while (j < N) {
      var dp = 0.0
      var n = j
      var i = 0
      while (i < M) {
        dp += row(i) * this(n)
        n += N
        i += 1
      }
      coordinates(j) = dp
      j += 1
    }
    new VectorRN(coordinates)
  }
  
  def transpose: MatrixRMxN = {
    val entries = new Array[Double](this.entries.length)
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
    new MatrixRMxN(N, M, entries)
  }
  
  /** Multiplies this $matrix by a compatible $matrix. ''M''x''N'' matrices
    * multiply ''N''x''P'' matrices to produce ''M''x''P'' matrix products.
    * 
    * @param  that  the $matrix to multiply by.
    * @return the matrix product of this $matrix and the other $matrix.
    */
  def * (that: MatrixRMxN): MatrixRMxN = {
    if (N != that.M) throw new DimensionException
    val P = that.N
    val entries = new Array[Double](M * P)
    var k = 0
    var i = 0
    var i0 = 0
    while (i < M) {
      var j = 0
      while (j < P) {
        var dp = 0.0
        var m = i0
        var n = j
        var d = 0
        while (d < N) {
          dp += this(m) * that(n)
          m += 1
          n += P
          d += 1
        }
        entries(k) = dp
        k += 1
        j += 1
      }
      i0 += N
      i += 1
    }
    new MatrixRMxN(M, P, entries)
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: MatrixRMxN =>
      var equal = M == that.M && N == that.N
      var k = 0
      while (k < entries.length && equal) {
        equal = this(k) == that(k)
        k += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    var h = 1138732008
    var k = 0
    while (k < entries.length) {
      mix(h, this(k))
      k += 1
    }
    mash(h)
  }
  
  override def toString: String = {
    val s = new StringBuilder("MatrixMxN")
    s.append('(').append(M).append(", ").append(N).append(')')
    s.append('(')
    if (entries.length > 0) {
      s.append(this(0))
      var k = 1
      var i = 0
      var j = 1
      while (i < M) {
        while (j < N) {
          if (k % N != 0) s.append(", ") else s.append(",  ")
          s.append(this(k))
          k += 1
          j += 1
        }
        i += 1
        j = 0
      }
    }
    s.append(')')
    s.toString
  }
}

/** Contains factory methods for ''M''x''N''-dimensional `Real` matrices. */
object MatrixRMxN {
  def apply(M: Int, N: Int, entries: Array[Double]): MatrixRMxN =
    new MatrixRMxN(M, N, entries)
  
  def apply(M: Int, N: Int)(entries: Double*): MatrixRMxN =
    new MatrixRMxN(M, N)(entries: _*)
  
  def columns(columns: VectorRN*): MatrixRMxN = {
    val N = columns.length
    val M = if (N > 0) columns(0).dimension else 0
    val entries = new Array[Double](M * N)
    var j = 0
    while (j < N) {
      val column = columns(j)
      if (M != column.dimension) throw new DimensionException
      var k = j
      var i = 0
      while (i < M) {
        entries(k) = column(i)
        k += N
        i += 1
      }
      j += 1
    }
    new MatrixRMxN(M, N, entries)
  }
  
  def rows(rows: VectorRN*): MatrixRMxN = {
    val M = rows.length
    val N = if (M > 0) rows(0).dimension else 0
    val entries = new Array[Double](M * N)
    var k = 0
    var i = 0
    while (i < M) {
      val row = rows(i)
      if (N != row.dimension) throw new DimensionException
      var j = 0
      while (j < N) {
        entries(k) = row(j)
        k += 1
        j += 1
      }
      i += 1
    }
    new MatrixRMxN(M, N, entries)
  }
}
