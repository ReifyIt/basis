/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.existentials

trait RealMatrixSpace
    [V <: RealVectorSpace with Singleton,
     W <: RealVectorSpace with Singleton]
  extends MatrixSpace[V, W, Real.type] {
  
  trait Element extends Any with super.Element { this: Matrix =>
    override protected def Matrix: RealMatrixSpace.this.type = RealMatrixSpace.this
    
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
    
    override def :* (scalar: Real): Matrix = {
      val entries = new Array[Double](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k).value * scalar.value
        k += 1
      }
      Matrix(entries)
    }
    
    override def *: (scalar: Real): Matrix = this :* scalar
    
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
    
    override def T: Transpose.Matrix = {
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
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        var dim = M * N
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
      import basis.util.MurmurHash._
      var dim = M * N
      var h = -1997372447
      var k = 0
      while (k < dim) {
        h = mix(h, this(k).value)
        k += 1
      }
      mash(h)
    }
  }
  
  override type Matrix <: Element
  
  override type Scalar = Real
  
  override val Transpose: RealMatrixSpace[W, V]
  
  override def Row: V
  
  override def Col: W
  
  override def Scalar = Real
  
  override def apply(entries: TraversableOnce[Real]): Matrix
  
  def apply(entries: Array[Double]): Matrix
  
  override def rows(vectors: TraversableOnce[Row]): Matrix = {
    val rows = vectors.toSeq
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
  
  override def cols(vectors: TraversableOnce[Col]): Matrix = {
    val cols = vectors.toSeq
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
  
  override def zero: Matrix = apply(new Array[Double](M * N))
  
  override def compose[U <: VectorSpace[Real.type] with Singleton]
      (that: MatrixSpace[U, V, Real.type]): MatrixSpace[U, W, Real.type] = {
    if (that.isInstanceOf[RealMatrixSpace[_, V]])
      compose(that.asInstanceOf[RealMatrixSpace[U with RealVectorSpace, V]]).asInstanceOf[MatrixSpace[U, W, Real.type]]
    else super.compose[U](that)
  }
  
  def compose[U <: RealVectorSpace with Singleton]
      (that: RealMatrixSpace[U, V]): RealMatrixSpace[U, W] =
    new RMxN[U, W](that.Row, Col)
  
  override def product[U <: VectorSpace[Real.type] with Singleton](
      matrixA: A#Matrix forSome { type A <: MatrixSpace[U, W, Real.type] },
      matrixB: B#Matrix forSome { type B <: MatrixSpace[V, U, Real.type] }): Matrix = {
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
