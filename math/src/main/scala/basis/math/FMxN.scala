//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math

/** An abstract ''M'' by ''N'' matrix space over a ring. Matrix spaces
  * describe linear maps between vector spaces relative to the vector spaces'
  * assumed bases. Matrix addition associates and commutes, and scalar
  * multiplication associates, commutes, and distributes over matrix addition
  * and scalar addition. Matrix and vector multiplication also both associate
  * and distribute over matrix addition. Vectors in the row space multiply as
  * columns on the right, and vectors in the column space multiply as rows on
  * the left. Addition and scalar multiplication both have an identity element,
  * and every matrix has an additive inverse. To the extent practicable, the
  * following axioms should hold.
  *
  * '''Axioms for matrix addition''':
  *   - if 𝐀 and 𝐁 are matrices in `this`, then their sum 𝐀 + 𝐁 is also a matrix in `this`.
  *   - 𝐀 + 𝐁 == 𝐁 + 𝐀 for all matrices 𝐀, 𝐁 in `this`.
  *   - (𝐀 + 𝐁) + 𝐂 == 𝐀 + (𝐁 + 𝐂) for all matrices 𝐀, 𝐁, 𝐂 in `this`.
  *   - `this` has a matrix `zero` such that `zero` + 𝐀 == 𝐀 for every matrix 𝐀 in `this`.
  *   - to every matrix 𝐀 in `this` corresponds a matrix -𝐀 in `this` such that 𝐀 + (-𝐀) == `zero`.
  *
  * '''Axioms for scalar multiplication''':
  *   - if 𝑥 is a scalar in `this` and 𝐀 is a matrix in `this`, then their product 𝑥 *: 𝐀 is also a matrix in `this`.
  *   - (𝑥 * 𝑦) *: 𝐀 == 𝑥 *: (𝑦 *: 𝐀) for all scalars 𝑥, 𝑦 and every matrix 𝐀 in `this`.
  *   - `Scalar` has an element `unit` such that `unit` *: 𝐀 == 𝐀 for every matrix 𝐀 in `this`.
  *
  * '''Axioms for vector multiplication''':
  *   - If 𝐀 is a matrix in `this` and 𝐯 is a vector in the row space of 𝐀, then their product
  *     𝐀 :⋅ 𝐯 is a vector in the column space of 𝐀.
  *   - (𝐀 ⋅ 𝐁) :⋅ 𝐯 == 𝐀 :⋅ (𝐁 :⋅ 𝐯) for all matrices 𝐀, 𝐁 and every vector 𝐯 in the row space of 𝐁,
  *     where the row space of 𝐀 equals the column space of 𝐁.
  *   - 𝐀 :⋅ 𝐯 == 𝐯 ⋅: 𝐀.`T` for every matrix 𝐀 and every vector 𝐯 in the row space of 𝐀.
  *
  * '''Axioms for matrix multiplication''':
  *   - if 𝐀 and 𝐁 are matrices and the row space of 𝐀 equals the column space of 𝐁,
  *     then the matrix product 𝐀 ⋅ 𝐁 exists.
  *   - (𝐀 ⋅ 𝐁) ⋅ 𝐂 == 𝐀 ⋅ (𝐁 ⋅ 𝐂) for all matrices 𝐀, 𝐁, 𝐂 where the row space of 𝐀 equals
  *     the column space of 𝐁, and the row space of 𝐁 equals the column space of 𝐂.
  *   - (𝐀 ⋅ 𝐁).`T` == 𝐁.`T` ⋅ 𝐀.`T` for all matrices 𝐀, 𝐁 where the row space of 𝐀 equals the column space of 𝐁.
  *
  * '''Distributive laws''':
  *   - 𝑥 *: (𝐀 + 𝐁) == (𝑥 *: 𝐀) + (𝑥 *: 𝐁) for every scalar 𝑥 and all matrices 𝐀, 𝐁 in `this`.
  *   - (𝑥 + 𝑦) *: 𝐀 == (𝑥 *: 𝐀) + (𝑦 *: 𝐀) for all scalars 𝑥, 𝑦 and every matrix 𝐀 in `this`.
  *   - 𝐯 ⋅: (𝐀 + 𝐁) == (𝐯 ⋅: 𝐀) + (𝐯 ⋅: 𝐁) for all matrices 𝐀, 𝐁 in the same matrix space, and every
  *     vector 𝐯 in the column space of 𝐀 and 𝐁.
  *   - (𝐀 + 𝐁) :⋅ 𝐯 == (𝐀 :⋅ 𝐯) + (𝐁 :⋅ 𝐯) for all matrices 𝐀, 𝐁 in the same matrix space, and every
  *     vector 𝐯 in the row space of 𝐀 and 𝐁.
  *   - 𝐀 ⋅ (𝐁 + 𝐂) == (𝐀 ⋅ 𝐁) + (𝐀 ⋅ 𝐂) for all matrices 𝐁, 𝐂 in the same matrix space, and every
  *     matrix 𝐀 whose row space equals the colum space of 𝐁 and 𝐂.
  *   - (𝐀 + 𝐁) ⋅ 𝐂 == (𝐀 ⋅ 𝐂) + (𝐁 ⋅ 𝐂) for all matrices 𝐀, 𝐁 in the same matrix space, and every
  *     matrix 𝐂 whose column space equals the row space of 𝐀 and 𝐁.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    MatrixSpaces
  *
  * @define space   matrix space
  */
trait FMxN extends VectorSpace { FMxN =>
  /** The type of vectors in this $space; equivalent to the type of matrices. */
  override type Vector = Matrix

  /** The type of matrices in this $space. */
  type Matrix <: MatrixFMxN

  /** The type of matrix transposes. */
  type Transpose = Transpose.Matrix

  /** Returns the transpose of this $space. */
  val Transpose: FMxN {
    val Row: FMxN.Col.type
    val Col: FMxN.Row.type
    val Scalar: FMxN.Scalar.type
  }

  /** The type of vectors in the row space. */
  type Row = Row.Vector

  /** Returns the row space. */
  val Row: FN { val Scalar: FMxN.Scalar.type }

  /** The type of vectors in the column space. */
  type Col = Col.Vector

  /* Returns the column space. */
  val Col: FN { val Scalar: FMxN.Scalar.type }

  implicit def ScalarTag: scala.reflect.ClassTag[Scalar]

  def dim: Int = Col.dim * Row.dim

  override def zero: Matrix = {
    val d = dim
    val z = Scalar.zero
    val entries = new Array[Scalar](d)
    var i = 0
    while (i < d) {
      entries(i) = z
      i += 1
    }
    apply(entries)
  }

  /** Returns the identity matrix of this $space, if one exists. */
  def unit: Matrix = {
    val m = Col.dim
    val n = Row.dim
    val z = Scalar.zero
    val u = Scalar.unit
    val entries = new Array[Scalar](m * n)
    var k = 0
    var i = 0
    var j = 0
    while (i < m) {
      while (j < n) {
        entries(k) = if (i != j) z else u
        k += 1
        j += 1
      }
      j = 0
      i += 1
    }
    apply(entries)
  }

  /** Returns a new matrix with the given row-major entries. */
  def apply(entries: Array[Scalar]): Matrix

  /** Returns a new matrix with the given rows. */
  def rows(rows: Row*): Matrix = {
    val m = Col.dim
    val n = Row.dim
    if (rows.length != m) throw new DimensionException
    val entries = new Array[Scalar](m * n)
    var k = 0
    var i = 0
    while (i < m) {
      val row = rows(i)
      if (row.dim != n) throw new DimensionException
      var j = 0
      while (j < n) {
        entries(k) = row(j)
        k += 1
        j += 1
      }
      i += 1
    }
    apply(entries)
  }

  /** Returns a new matrix with the given columns. */
  def cols(cols: Col*): Matrix = {
    val m = Col.dim
    val n = Row.dim
    if (cols.length != n) throw new DimensionException
    val entries = new Array[Scalar](m * n)
    var j = 0
    while (j < n) {
      val col = cols(j)
      if (col.dim != m) throw new DimensionException
      var k = j
      var i = 0
      while (i < m) {
        entries(k) = col(i)
        k += n
        i += 1
      }
      j += 1
    }
    apply(entries)
  }

  /** Returns the matrix product of the first matrix, whose column space equals
    * this column space, times the second matrix, whose row space equals this
    * row space, where the row space of the first matrix equals the column
    * space of the second matrix. */
  def product
      [A <: FMxN { val Scalar: FMxN.Scalar.type },
       B <: FMxN { val Scalar: FMxN.Scalar.type }]
      (matrixA: A#MatrixFMxN, matrixB: B#MatrixFMxN): Matrix = {
    val M = matrixA.Col.dim
    val N = matrixA.Row.dim
    if (N == matrixB.Col.dim) throw new DimensionException
    val P = matrixB.Row.dim
    val entries = new Array[Scalar](M * P)
    var k = 0
    var i = 0
    var i0 = 0
    while (i < M) {
      var j = 0
      while (j < P) {
        var s = Scalar.zero
        var m = i0
        var n = j
        var d = 0
        while (d < N) {
          s += matrixA(m) * matrixB(n)
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
    apply(entries)
  }

  /** A matrix in this $space.
    *
    * @define vector  $matrix
    * @define matrix  matrix
    */
  trait MatrixFMxN extends Any with VectorElement {
    /** Returns the row space of this $matrix. */
    def Row: FMxN.Row.type = FMxN.Row

    /** Returns the column space of this $matrix. */
    def Col: FMxN.Col.type = FMxN.Col

    /** Returns the $scalar at the given row-major index. */
    def apply(k: Int): Scalar

    /** Returns the $scalar in row `i`, column `j`. */
    def apply(i: Int, j: Int): Scalar = {
      val m = Col.dim
      val n = Row.dim
      if (i < 0 || i >= m || j < 0 || j >= n)
        throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(n * i + j)
    }

    /** Returns the row at the given index. */
    def row(i: Int): Row = {
      val n = Row.dim
      if (i < 0 || i >= Col.dim) throw new IndexOutOfBoundsException("row "+ i)
      val coords = new Array[Scalar](n)
      var j = 0
      var k = n * i
      while (j < n) {
        coords(j) = this(k)
        j += 1
        k += 1
      }
      Row(coords)
    }

    /** Returns the column at the given index. */
    def col(j: Int): Col = {
      val m = Col.dim
      val n = Row.dim
      if (j < 0 || j >= n) throw new IndexOutOfBoundsException("col "+ j)
      val coords = new Array[Scalar](m)
      var i = 0
      var k = j
      while (i < m) {
        coords(i) = this(k)
        i += 1
        k += n
      }
      Col(coords)
    }

    override def + (that: Matrix): Matrix = {
      val m = Col.dim
      val n = Row.dim
      val d = m * n
      if (m != that.Col.dim || n != that.Row.dim) throw new DimensionException
      val entries = new Array[Scalar](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k) + that(k)
        k += 1
      }
      FMxN(entries)
    }

    override def unary_- : Matrix = {
      val d = Col.dim * Row.dim
      val entries = new Array[Scalar](d)
      var k = 0
      while (k < d) {
        entries(k) = -this(k)
        k += 1
      }
      FMxN(entries)
    }

    override def - (that: Matrix): Matrix = {
      val m = Col.dim
      val n = Row.dim
      val d = m * n
      if (m != that.Col.dim || n != that.Row.dim) throw new DimensionException
      val entries = new Array[Scalar](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k) - that(k)
        k += 1
      }
      FMxN(entries)
    }

    override def :* (scalar: Scalar): Matrix = {
      val d = Col.dim * Row.dim
      val entries = new Array[Scalar](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k) * scalar
        k += 1
      }
      FMxN(entries)
    }

    override def *: (scalar: Scalar): Matrix = {
      val d = Col.dim * Row.dim
      val entries = new Array[Scalar](d)
      var k = 0
      while (k < d) {
        entries(k) = scalar * this(k)
        k += 1
      }
      FMxN(entries)
    }

    override def ∘ (that: Matrix): Matrix = {
      val m = Col.dim
      val n = Row.dim
      val d = m * n
      if (m != that.Col.dim || n != that.Row.dim) throw new DimensionException
      val entries = new Array[Scalar](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k) * that(k)
        k += 1
      }
      FMxN(entries)
    }

    /** Returns a $vector in the column space by right-multiplying this $matrix
      * by a $vector in the row space. The name of this method contains
      * the unicode dot operator (U+22C5). */
    def :⋅ (vector: Row): Col = {
      val m = Col.dim
      val n = Row.dim
      if (n != vector.dim) throw new DimensionException
      val coords = new Array[Scalar](m)
      var i = 0
      var i0 = 0
      while (i < m) {
        var s = Scalar.zero
        var k = i0
        var j = 0
        while (j < n) {
          s += this(k) * vector(j)
          k += 1
          j += 1
        }
        coords(i) = s
        i += 1
        i0 += n
      }
      Col(coords)
    }

    /** Returns a $vector in the row space by left-multiplying a $vector in
      * the column space by this $matrix. The name of this method contains
      * the unicode dot operator (U+22C5). */
    def ⋅: (vector: Col): Row = {
      val m = Col.dim
      val n = Row.dim
      if (vector.dim != m) throw new DimensionException
      val coords = new Array[Scalar](n)
      var j = 0
      while (j < n) {
        var s = Scalar.zero
        var k = j
        var i = 0
        while (i < m) {
          s += vector(i) * this(k)
          k += n
          i += 1
        }
        coords(j) = s
        j += 1
      }
      Row(coords)
    }

    /** Returns the inverse of this $matrix, if it exists. */
    def inverse: Matrix = scala.sys.error("not implemented")

    /** Returns the transpose of this $matrix. */
    def transpose: Transpose = {
      val m = Col.dim
      val n = Row.dim
      val entries = new Array[Scalar](m * n)
      var k = 0
      var j = 0
      while (j < n) {
        var l = j
        var i = 0
        while (i < m) {
          entries(k) = this(l)
          l += n
          k += 1
          i += 1
        }
        j += 1
      }
      Transpose(entries)
    }

    /** Returns the determinant of this $matrix. */
    def det: Scalar = scala.sys.error("not implemented")

    /** Returns the trace of this $matrix. */
    def trace: Scalar = {
      val m = Col.dim
      val n = Row.dim
      if (m != n) throw new DimensionException
      val d = m * n
      var s = Scalar.zero
      var k = 0
      while (k < d) {
        s += this(k)
        k += n + 1
      }
      s
    }

    override def equals(other: Any): Boolean = other match {
      case that: MatrixFMxN =>
        val m = Col.dim
        val n = Row.dim
        var equal = m == that.Col.dim && n == that.Row.dim
        val d = m * n
        var k = 0
        while (k < d && equal) {
          equal = this(k).equals(that(k))
          k += 1
        }
        equal
      case _ => false
    }

    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      val d = Col.dim * Row.dim
      var h = -1997372447
      var k = 0
      while (k < d) {
        h = mix(h, this(k).##)
        k += 1
      }
      finalizeHash(h, d)
    }

    override def toString: String = {
      val m = Col.dim
      val n = Row.dim
      val s = new java.lang.StringBuilder(FMxN.toString)
      s.append('(')
      if (m > 0 && n > 0) {
        s.append(this(0))
        var k = 1
        var j = 1
        var i = 0
        while (i < m) {
          while (j < n) {
            (if (j != 0) s.append(", ") else s.append(",  ")).append(this(k))
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
}
