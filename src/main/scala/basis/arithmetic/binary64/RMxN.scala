/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.arithmetic
package binary64

/** An abstract ''M'' by ''N'' real matrix space.
  * 
  * @author Chris Sachs
  * 
  * @tparam V   The row space.
  * @tparam W   The column space.
  * 
  * @define space   real matrix space
  */
trait RMxN[V <: RN with Singleton, W <: RN with Singleton] extends FMxN[V, W, Binary64.type] {
  import scala.language.existentials
  
  trait Element extends Any with super.Element {
    override protected def Matrix: RMxN.this.type = RMxN.this
    
    override def apply(k: Int): Scalar
    
    override def apply(i: Int, j: Int): Scalar = {
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
        coords(j) = this(n).value
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
        coords(i) = this(m).value
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
        entries(k) = this(k).value + that(k).value
        k += 1
      }
      Matrix(entries)
    }
    
    override def unary_- : Matrix = {
      val entries = new Array[Double](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = -this(k).value
        k += 1
      }
      Matrix(entries)
    }
    
    override def - (that: Matrix): Matrix = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[Double](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k).value - that(k).value
        k += 1
      }
      Matrix(entries)
    }
    
    override def :* (scalar: Scalar): Matrix = {
      val entries = new Array[Double](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k).value * scalar.value
        k += 1
      }
      Matrix(entries)
    }
    
    override def *: (scalar: Scalar): Matrix = this :* scalar
    
    override def :⋅ (vector: Row): Col = {
      if (N != vector.N) throw new DimensionException
      val coords = new Array[Double](M)
      var i = 0
      var i0 = 0
      while (i < coords.length) {
        var s = 0.0
        var n = i0
        var j = 0
        while (j < N) {
          s += this(n).value * vector(j).value
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
      if (vector.N != M) throw new DimensionException
      val coords = new Array[Double](N)
      var j = 0
      while (j < coords.length) {
        var s = 0.0
        var n = j
        var i = 0
        while (i < M) {
          s += vector(i).value * this(n).value
          n += N
          i += 1
        }
        coords(j) = s
        j += 1
      }
     Row(coords)
    }
    
    def ⋅ [U <: RN with Singleton]
        (that: B#Element forSome { type B <: RMxN[U, V] })
      : C#Matrix forSome { type C <: RMxN[U, W] } =
      compose(that.Matrix).product(this, that)
    
    override def T: Transpose#Matrix = {
      val entries = new Array[Double](N * M)
      var k = 0
      var j = 0
      while (j < N) {
        var n = j
        var i = 0
        while (i < M) {
          entries(k) = this(n).value
          n += N
          k += 1
          i += 1
        }
        j += 1
      }
      Transpose(entries)
    }
    
    override def det(implicit isSquare: V =:= W): Scalar = sys.error("not implemented")
    
    override def trace(implicit isSquare: V =:= W): Scalar = {
      assume(M == N)
      val dim = M * N
      var s = 0.0
      var k = 0
      while (k < dim) {
        s += this(k).value
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
          equal = this(k).value == that(k).value
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
  }
  
  override type Matrix <: Element
  
  override type Transpose <: RMxN[W, V] with Singleton { type Transpose = RMxN.this.type }
  
  override type Scalar = Binary64
  
  override def Transpose: Transpose
  
  override def Row: V
  
  override def Col: W
  
  override def Scalar: Binary64.type = Binary64
  
  override def zero: Matrix = apply(new Array[Double](M * N))
  
  override def identity(implicit isSquare: V =:= W): Matrix = {
    val entries = new Array[Double](M * N)
    var k = 0
    var i = 0
    var j = 0
    while (i < M) {
      while (j < N) {
        entries(k) = if (i != j) 0.0 else 1.0
        k += 1
        j += 1
      }
      j = 0
      i += 1
    }
    apply(entries)
  }
  
  def apply(entries: Array[Double]): Matrix
  
  override def apply(entries: Scalar*): Matrix = apply(entries.map(_.toDouble).toArray[Double])
  
  override def rows(rows: Row*): Matrix = {
    if (rows.length != M) throw new DimensionException
    val entries = new Array[Double](M * N)
    var k = 0
    var i = 0
    while (i < M) {
      val row = rows(i)
      if (row.N != N) throw new DimensionException
      var j = 0
      while (j < N) {
        entries(k) = row(j)
        k += 1
        j += 1
      }
      i += 1
    }
    apply(entries)
  }
  
  override def cols(cols: Col*): Matrix = {
    if (cols.length != N) throw new DimensionException
    val entries = new Array[Double](M * N)
    var j = 0
    while (j < N) {
      val col = cols(j)
      if (col.N != M) throw new DimensionException
      var k = j
      var i = 0
      while (i < M) {
        entries(k) = col(i)
        k += N
        i += 1
      }
      j += 1
    }
    apply(entries)
  }
  
  override def compose[U <: FN[Binary64.type] with Singleton]
      (that: FMxN[U, V, Binary64.type]): FMxN[U, W, Binary64.type] = {
    if (that.isInstanceOf[RMxN[_, V]])
      compose(that.asInstanceOf[RMxN[U with RN, V]]).asInstanceOf[FMxN[U, W, Binary64.type]]
    else super.compose(that)
  }
  
  /** Returns a real matrix space that maps the row space of another real
    * matrix space to this column space. */
  def compose[U <: RN with Singleton](that: RMxN[U, V]): RMxN[U, W] = that.Row map Col
  
  /** Returns the real matrix product of the first real matrix, whose column
    * space equals this column space, times the second real matrix, whose row
    * space equals this row space, where the row space of the first matrix
    * equals the column space of the second matrix. */
  override def product[U <: FN[Binary64.type] with Singleton](
      matrixA: A#Element forSome { type A <: FMxN[U, W, Binary64.type] },
      matrixB: B#Element forSome { type B <: FMxN[V, U, Binary64.type] }): Matrix = {
    if (matrixA.isInstanceOf[Element] && matrixB.isInstanceOf[Element]) {
      val realMatrixA = matrixA.asInstanceOf[Element]
      val realMatrixB = matrixB.asInstanceOf[Element]
      val M = realMatrixA.M
      val N = realMatrixA.N
      if (N != realMatrixB.M) throw new DimensionException
      val P = realMatrixB.N
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
          var d = 0
          while (d < N) {
            s += realMatrixA(m).value * realMatrixB(n).value
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
    else super.product[U](matrixA, matrixB)
  }
}

object RMxN {
  /** Returns an ''M'' by ''N'' real matrix space. */
  def apply(Row: RN, Col: RN): RMxN[Row.type, Col.type] =
    new Space[Row.type, Col.type](Row, Col)
  
  /** An ''M'' by ''N'' real matrix space.
    * 
    * @tparam V   The row space.
    * @tparam W   The column space.
    */
  private final class Space[V <: RN with Singleton, W <: RN with Singleton]
      (val Row: V, val Col: W)
    extends RMxN[V, W] {
    
    final class Element(entries: Array[Double]) extends super.Element {
      if (entries.length != M * N) throw new DimensionException
      
      override def apply(k: Int): Scalar = entries(k)
    }
    
    override type Matrix = Element
    
    private var _Transpose: Space[W, V] = null
    override def Transpose: Transpose = synchronized {
      if (_Transpose == null) {
        _Transpose = new Space[W, V](Col, Row)
        _Transpose._Transpose = this
      }
      _Transpose.asInstanceOf[Transpose]
    }
    
    override lazy val zero: Matrix = super.zero
    
    override def apply(entries: Array[Double]): Matrix = new Matrix(entries)
    
    override def toString: String = "RMxN"+"("+ Row +", "+ Col +")"
  }
}
