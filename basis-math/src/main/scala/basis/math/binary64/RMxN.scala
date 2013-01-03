/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** An abstract double-precision floating-point matrix space.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Real
  */
trait RMxN extends FMxN {
  trait Value extends Any with super.Value {
    override def Row: RMxN.this.Row.type = RMxN.this.Row
    override def Col: RMxN.this.Col.type = RMxN.this.Col
    
    override def apply(k: Int): Scalar
    
    override def apply(i: Int, j: Int): Scalar = {
      val m = Col.dim
      val n = Row.dim
      if (i < 0 || i >= m || j < 0 || j >= n)
        throw new java.lang.IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(n * i + j)
    }
    
    override def row(i: Int): Row = {
      val n = Row.dim
      if (i < 0 || i >= Col.dim) throw new java.lang.IndexOutOfBoundsException("row "+ i)
      val coords = new Array[Double](n)
      var j = 0
      var k = n * i
      while (j < n) {
        coords(j) = this(k).value
        j += 1
        k += 1
      }
      Row(coords)
    }
    
    override def col(j: Int): Col = {
      val m = Col.dim
      val n = Row.dim
      if (j < 0 || j >= n) throw new java.lang.IndexOutOfBoundsException("col "+ j)
      val coords = new Array[Double](m)
      var i = 0
      var k = j
      while (i < m) {
        coords(i) = this(k).value
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
      val entries = new Array[Double](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k).value + that(k).value
        k += 1
      }
      RMxN.this.apply(entries)
    }
    
    override def unary_- : Matrix = {
      val d = Col.dim * Row.dim
      val entries = new Array[Double](d)
      var k = 0
      while (k < d) {
        entries(k) = -this(k).value
        k += 1
      }
      RMxN.this.apply(entries)
    }
    
    override def - (that: Matrix): Matrix = {
      val m = Col.dim
      val n = Row.dim
      val d = m * n
      if (m != that.Col.dim || n != that.Row.dim) throw new DimensionException
      val entries = new Array[Double](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k).value - that(k).value
        k += 1
      }
      RMxN.this.apply(entries)
    }
    
    override def :* (scalar: Scalar): Matrix = {
      val d = Col.dim * Row.dim
      val entries = new Array[Double](d)
      var k = 0
      while (k < d) {
        entries(k) = this(k).value * scalar.value
        k += 1
      }
      RMxN.this.apply(entries)
    }
    
    override def *: (scalar: Scalar): Matrix = this :* scalar
    
    override def :⋅ (vector: Row): Col = {
      val m = Col.dim
      val n = Row.dim
      if (n != vector.dim) throw new DimensionException
      val coords = new Array[Double](m)
      var i = 0
      var i0 = 0
      while (i < m) {
        var s = 0.0
        var k = i0
        var j = 0
        while (j < n) {
          s += this(k).value * vector(j).value
          k += 1
          j += 1
        }
        coords(i) = s
        i += 1
        i0 += n
      }
      Col(coords)
    }
    
    override def ⋅: (vector: Col): Row = {
      val m = Col.dim
      val n = Row.dim
      if (vector.dim != m) throw new DimensionException
      val coords = new Array[Double](n)
      var j = 0
      while (j < n) {
        var s = 0.0
        var k = j
        var i = 0
        while (i < m) {
          s += vector(i).value * this(k).value
          k += n
          i += 1
        }
        coords(j) = s
        j += 1
      }
     Row(coords)
    }
    
    override def transpose: Transpose = {
      val m = Col.dim
      val n = Row.dim
      val entries = new Array[Double](m * n)
      var k = 0
      var j = 0
      while (j < n) {
        var l = j
        var i = 0
        while (i < m) {
          entries(k) = this(l).value
          l += n
          k += 1
          i += 1
        }
        j += 1
      }
      Transpose(entries)
    }
    
    override def det: Scalar = scala.sys.error("not implemented")
    
    override def trace: Scalar = {
      val m = Col.dim
      val n = Row.dim
      if (m != n) throw new DimensionException
      val d = m * n
      var s = 0.0
      var k = 0
      while (k < d) {
        s += this(k).value
        k += n + 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Value =>
        val m = Col.dim
        val n = Row.dim
        var equal = m == that.Col.dim && n == that.Row.dim
        val d = m * n
        var k = 0
        while (k < d && equal) {
          equal = this(k).value == that(k).value
          k += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      var d = Col.dim * Row.dim
      var h = -1997372447
      var k = 0
      while (k < d) {
        h = mix(h, this(k).##)
        k += 1
      }
      finalizeHash(h, d)
    }
  }
  
  override type Matrix <: Value
  
  override val Transpose: RMxN {
    val Row: RMxN.this.Col.type
    val Col: RMxN.this.Row.type
  }
  
  override val Row: RN
  
  override val Col: RN
  
  override type Scalar = Real
  
  override val Scalar: Real.type
  
  implicit override def ScalarTag: scala.reflect.ClassTag[Real] =
    scala.reflect.classTag[Real]
  
  override def zero: Matrix = apply(new Array[Double](dim))
  
  override def unit: Matrix = {
    val m = Col.dim
    val n = Row.dim
    val entries = new Array[Double](m * n)
    var k = 0
    var i = 0
    var j = 0
    while (i < m) {
      while (j < n) {
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
  
  override def apply(entries: Array[Scalar]): Matrix = {
    val d = dim
    if (entries.length != d) throw new DimensionException
    val ks = new Array[Double](d)
    var i = 0
    while (i < d) {
      ks(i) = entries(i).value
      i += 1
    }
    apply(ks)
  }
  
  override def rows(rows: Row*): Matrix = {
    val m = Col.dim
    val n = Row.dim
    if (rows.length != m) throw new DimensionException
    val entries = new Array[Double](m * n)
    var k = 0
    var i = 0
    while (i < m) {
      val row = rows(i)
      if (row.dim != n) throw new DimensionException
      var j = 0
      while (j < n) {
        entries(k) = row(j).value
        k += 1
        j += 1
      }
      i += 1
    }
    apply(entries)
  }
  
  override def cols(cols: Col*): Matrix = {
    val m = Col.dim
    val n = Row.dim
    if (cols.length != n) throw new DimensionException
    val entries = new Array[Double](m * n)
    var j = 0
    while (j < n) {
      val col = cols(j)
      if (col.dim != m) throw new DimensionException
      var k = j
      var i = 0
      while (i < m) {
        entries(k) = col(i).value
        k += n
        i += 1
      }
      j += 1
    }
    apply(entries)
  }
  
  /** Returns the real matrix product of the first real matrix, whose column
    * space equals this column space, times the second real matrix, whose row
    * space equals this row space, where the row space of the first matrix
    * equals the column space of the second matrix. */
  override def product
      [A <: FMxN { val Scalar: Real.type },
       B <: FMxN { val Scalar: Real.type }]
      (matrixA: A#Value, matrixB: B#Value): Matrix = {
    if (matrixA.isInstanceOf[Value] && matrixB.isInstanceOf[Value]) {
      val realMatrixA = matrixA.asInstanceOf[Value]
      val realMatrixB = matrixB.asInstanceOf[Value]
      val M = realMatrixA.Col.dim
      val N = realMatrixA.Row.dim
      if (N != realMatrixB.Col.dim) throw new DimensionException
      val P = realMatrixB.Row.dim
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
    else super.product(matrixA, matrixB)
  }
}
