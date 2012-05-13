/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import language.existentials

trait MatrixSpace
    [V <: VectorSpace[S] with Singleton,
     W <: VectorSpace[S] with Singleton,
     S <: Ring with Singleton]
  extends LinearSpace[S] {
  
  trait Element extends Any with super.Element { this: Matrix =>
    protected def Matrix: MatrixSpace.this.type = MatrixSpace.this
    
    def M: Int = Matrix.M
    
    def N: Int = Matrix.N
    
    def apply(k: Int): Scalar
    
    def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= M || j < 0 || j >= N)
        throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(N * i + j)
    }
    
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
      Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
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
      Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def + (that: Matrix): Matrix = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (this(k) + that(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
    }
    
    override def unary_- : Matrix = {
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (-this(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
    }
    
    override def - (that: Matrix): Matrix = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (this(k) - that(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
    }
    
    override def :* (scalar: Scalar): Matrix = {
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (this(k) * scalar).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
    }
    
    override def *: (scalar: Scalar): Matrix = {
      val entries = new Array[AnyRef](M * N)
      var k = 0
      while (k < entries.length) {
        entries(k) = (scalar * this(k)).asInstanceOf[AnyRef]
        k += 1
      }
      Matrix(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
    }
    
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
      Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
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
      Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    def ⋅ [U <: VectorSpace[S] with Singleton]
        (that: B#Matrix forSome { type B <: MatrixSpace[U, V, S] })
      : C#Matrix forSome { type C <: MatrixSpace[U, W, S] } =
      compose(that.Matrix).product(this, that)
    
    def T: Transpose.Matrix = {
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
      Transpose(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        var dim = M * N
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
      import basis.util.MurmurHash._
      var dim = M * N
      var h = -1997372447
      var k = 0
      while (k < dim) {
        h = mix(h, this(k))
        k += 1
      }
      mash(h)
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
  
  override type Vector = Matrix
  
  type Matrix <: Element
  
  type Row = V#Vector
  
  type Col = W#Vector
  
  val Transpose: MatrixSpace[W, V, S]
  
  def Row: V
  
  def Col: W
  
  def M: Int = Col.N
  
  def N: Int = Row.N
  
  def apply(entries: TraversableOnce[Scalar]): Matrix
  
  def rows(vectors: TraversableOnce[Row]): Matrix = {
    val rows = vectors.toSeq
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
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def cols(vectors: TraversableOnce[Col]): Matrix = {
    val cols = vectors.toSeq
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
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  override def zero: Matrix = {
    val z = Scalar.zero.asInstanceOf[AnyRef]
    val entries = new Array[AnyRef](M * N)
    var i = 0
    while (i < entries.length) {
      entries(i) = z
      i += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def compose[U <: VectorSpace[S] with Singleton]
      (that: MatrixSpace[U, V, S]): MatrixSpace[U, W, S] =
    new generic.FMxN[U, W, S](Scalar)(that.Row, Col)
  
  def product[U <: VectorSpace[S] with Singleton](
      matrixA: A#Matrix forSome { type A <: MatrixSpace[U, W, S] },
      matrixB: B#Matrix forSome { type B <: MatrixSpace[V, U, S] }): Matrix = {
    val M = matrixA.M
    val N = matrixA.N
    if (N != matrixB.M) throw new DimensionException
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
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
}
