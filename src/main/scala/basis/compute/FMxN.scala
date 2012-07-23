/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.compute

import basis.algebra._

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
  * @author Chris Sachs
  * 
  * @tparam V   The row space.
  * @tparam W   The column space.
  * @tparam S   The set of scalars.
  * 
  * @define space   matrix space
  */
trait FMxN[V <: FN[S] with Singleton, W <: FN[S] with Singleton, S <: Ring with Singleton] extends VectorSpace[S] {
  import scala.language.existentials
  
  /** A matrix in this $space.
    * 
    * @define vector  $matrix
    * @define matrix  matrix
    */
  trait Element extends Any with super.Element {
    protected def Matrix: FMxN.this.type = FMxN.this
    
    /** Returns the number of rows, or equivalently, the dimension of the columns. */
    def M: Int = Matrix.M
    
    /** Returns the number of columns, or equivalently, the dimension of the rows. */
    def N: Int = Matrix.N
    
    /** Returns the entry at the given row-major index. */
    def apply(k: Int): Scalar
    
    /** Returns the entry in row `i`, column `j`. */
    def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= M || j < 0 || j >= N)
        throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(N * i + j)
    }
    
    /** Returns the row at the given index. */
    def row(i: Int): Row = {
      if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
      val coords = new Array[AnyRef](N)
      var j = 0
      var n = N * i
      while (j < coords.length) {
        coords(j) = this(n).asInstanceOf[AnyRef]
        j += 1
        n += 1
      }
      Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    /** Returns the column at the given index. */
    def col(j: Int): Col = {
      if (j < 0 || j >= N) throw new IndexOutOfBoundsException("col "+ j)
      val coords = new Array[AnyRef](M)
      var i = 0
      var m = j
      while (i < coords.length) {
        coords(i) = this(m).asInstanceOf[AnyRef]
        i += 1
        m += N
      }
      Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def + (that: Matrix): Matrix = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (this(k) + that(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def unary_- : Matrix = {
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (-this(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def - (that: Matrix): Matrix = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (this(k) - that(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def :* (scalar: Scalar): Matrix = {
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (this(k) * scalar).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def *: (scalar: Scalar): Matrix = {
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (scalar * this(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    /** Returns a vector in the column space by right-multiplying this $matrix
      * by a column vector in the row space. The name of this method contains
      * the unicode dot operator (U+22C5). */
    def :⋅ (vector: Row): Col = {
      if (N != vector.N) throw new DimensionException
      val coords = new Array[AnyRef](M)
      var i = 0
      var i0 = 0
      while (i < coords.length) {
        var s = Scalar.zero
        var n = i0
        var j = 0
        while (j < N) {
          s += this(n) * vector(j)
          n += 1
          j += 1
        }
        coords(i) = s.asInstanceOf[AnyRef]
        i += 1
        i0 += N
      }
      Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    /** Returns a vector in the row space by left-multiplying a row vector in
      * the column space by this $matrix. The name of this method contains
      * the unicode dot operator (U+22C5). */
    def ⋅: (vector: Col): Row = {
      if (vector.N != M) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var j = 0
      while (j < coords.length) {
        var s = Scalar.zero
        var n = j
        var i = 0
        while (i < M) {
          s += vector(i) * this(n)
          n += N
          i += 1
        }
        coords(j) = s.asInstanceOf[AnyRef]
        j += 1
      }
      Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    /** Returns the product of this $matrix times another $matrix whose column
      * space equals this row space. The product matrix exists in some matrix
      * space whose row space equals the multiplier's row space, and whose
      * column space equals this column space. The name of this method
      * contains the unicode dot operator (U+22C5). */
    def ⋅ [U <: FN[S] with Singleton]
        (that: B#Element forSome { type B <: FMxN[U, V, S] })
      : C#Matrix forSome { type C <: FMxN[U, W, S] } =
      compose(that.Matrix).product(this, that)
    
    /** Returns the inverse of this square matrix, if one exists.
      * 
      * @usecase def inverse: Option[Matrix]
      *   @inheritdoc
      */
    def inverse(implicit isSquare: V =:= W): Option[Matrix] = sys.error("not implemented")
    
    /** Returns the transpose of this $matrix. */
    def T: Transpose#Matrix = {
      val entries = new Array[AnyRef](N * M)
      var k = 0
      var j = 0
      while (j < N) {
        var n = j
        var i = 0
        while (i < M) {
          entries(k) = this(n).asInstanceOf[AnyRef]
          n += N
          k += 1
          i += 1
        }
        j += 1
      }
      Transpose(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    /** Returns the determinant of this square $matrix.
      * 
      * @usecase def det: Scalar
      *   @inheritdoc
      */
    def det(implicit isSquare: V =:= W): Scalar = sys.error("not implemented")
    
    /** Returns the trace of this square $matrix.
      * 
      * @usecase def trace: Scalar
      *   @inheritdoc
      */
    def trace(implicit isSquare: V =:= W): Scalar = {
      assume(M == N)
      val dim = M * N
      var s = Scalar.zero
      var k = 0
      while (k < dim) {
        s += this(k)
        k += N + 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        val dim = M * N
        var equal = M == that.M && N == that.N
        var k = 0
        while (k < dim && equal) {
          equal = this(k).equals(that(k))
          k += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      var dim = M * N
      var h = -1997372447
      var k = 0
      while (k < dim) {
        h = mix(h, this(k).##)
        k += 1
      }
      finalizeHash(h, dim)
    }
    
    override def toString: String = {
      val s = new StringBuilder(Matrix.toString)
      s.append('(')
      if (M > 0 && N > 0) {
        s.append(this(0))
        var k = 1
        var j = 1
        var i = 0
        while (i < M) {
          while (j < N) {
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
  
  /** The type of vectors in this $space; equivalent to the type of matrices. */
  override type Vector = Matrix
  
  /** The type of matrices in this $space. */
  type Matrix <: Element
  
  /** The transpose of this $space. */
  type Transpose <: FMxN[W, V, S] with Singleton { type Transpose = FMxN.this.type }
  
  /** The type of vectors in the row space. */
  type Row = V#Vector
  
  /** The type of vectors in the column space. */
  type Col = W#Vector
  
  /** Returns the transpose of this $space. */
  def Transpose: Transpose
  
  /** Returns the row space. */
  def Row: V
  
  /** Returns the column space. */
  def Col: W
  
  /** Returns the dimension of the column space. */
  def M: Int = Col.N
  
  /** Returns the dimension of the row space. */
  def N: Int = Row.N
  
  override def zero: Matrix = {
    val z = Scalar.zero.asInstanceOf[AnyRef]
    val entries = new Array[AnyRef](M * N)
    var i = 0
    while (i < entries.length) {
      entries(i) = z
      i += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
  }
  
  /** Returns the identity matrix of this $space, if one exists.
    * 
    * @usecase def identity: Matrix
    *   @inheritdoc
    */
  def identity(implicit isSquare: V =:= W): Matrix = {
    val z = Scalar.zero.asInstanceOf[AnyRef]
    val u = Scalar.unit.asInstanceOf[AnyRef]
    val entries = new Array[AnyRef](M * N)
    var k = 0
    var i = 0
    var j = 0
    while (i < M) {
      while (j < N) {
        entries(k) = if (i != j) z else u
        k += 1
        j += 1
      }
      j = 0
      i += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
  }
  
  /** Returns a new matrix with the given row-major entries. */
  def apply(entries: Scalar*): Matrix
  
  /** Returns a new matrix with the given rows. */
  def rows(rows: Row*): Matrix = {
    if (rows.length != M) throw new DimensionException
    val entries = new Array[AnyRef](M * N)
    var k = 0
    var i = 0
    while (i < M) {
      val row = rows(i)
      if (row.N != N) throw new DimensionException
      var j = 0
      while (j < N) {
        entries(k) = row(j).asInstanceOf[AnyRef]
        k += 1
        j += 1
      }
      i += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
  }
  
  /** Returns a new matrix with the given columns. */
  def cols(cols: Col*): Matrix = {
    if (cols.length != N) throw new DimensionException
    val entries = new Array[AnyRef](M * N)
    var j = 0
    while (j < N) {
      val col = cols(j)
      if (col.N != M) throw new DimensionException
      var k = j
      var i = 0
      while (i < M) {
        entries(k) = col(i).asInstanceOf[AnyRef]
        k += N
        i += 1
      }
      j += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
  }
  
  /** Returns a matrix space that maps the row space of another matrix space
    * to this column space. */
  def compose[U <: FN[S] with Singleton](that: FMxN[U, V, S]): FMxN[U, W, S] = that.Row map Col
  
  /** Returns the matrix product of the first matrix, whose column space equals
    * this column space, times the second matrix, whose row space equals this
    * row space, where the row space of the first matrix equals the column
    * space of the second matrix. */
  def product[U <: FN[S] with Singleton](
      matrixA: A#Element forSome { type A <: FMxN[U, W, S] },
      matrixB: B#Element forSome { type B <: FMxN[V, U, S] }): Matrix = {
    val M = matrixA.M
    val N = matrixA.N
    assume(N == matrixB.M)
    val P = matrixB.N
    val entries = new Array[AnyRef](M * P)
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
        entries(k) = s.asInstanceOf[AnyRef]
        k += 1
        j += 1
      }
      i += 1
      i0 += N
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
  }
}

object FMxN {
  /** Returns an ''M'' by ''N'' matrix space over the given ring. */
  def apply(Scalar: Ring)(Row: FN[Scalar.type], Col: FN[Scalar.type]): FMxN[Row.type, Col.type, Scalar.type] =
    new Space[Row.type, Col.type, Scalar.type](Scalar)(Row, Col)
  
  /** A generic ''M'' by ''N'' matrix space over a ring.
    * 
    * @tparam V   The row space.
    * @tparam W   The column space.
    * @tparam S   The set of scalars.
    */
  private final class Space[V <: FN[S] with Singleton, W <: FN[S] with Singleton, S <: Ring with Singleton]
      (val Scalar: S)(val Row: V, val Col: W)
    extends FMxN[V, W, S] {
    
    final class Element(entries: Array[AnyRef]) extends super.Element {
      if (entries.length != M * N) throw new DimensionException
      
      override def apply(k: Int): Scalar = entries(k).asInstanceOf[Scalar]
    }
    
    override type Matrix = Element
    
    private var _Transpose: Space[W, V, S] = null
    override def Transpose: Transpose = synchronized {
      if (_Transpose == null) {
        _Transpose = new Space[W, V, S](Scalar)(Col, Row)
        _Transpose._Transpose = this
      }
      _Transpose.asInstanceOf[Transpose]
    }
    
    override lazy val zero: Matrix = super.zero
    
    override def apply(entries: Scalar*): Matrix =
      new Matrix(entries.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
    
    override def toString: String = "FMxN"+"("+ Scalar +")"+"("+ Row +", "+ Col +")"
  }
}
