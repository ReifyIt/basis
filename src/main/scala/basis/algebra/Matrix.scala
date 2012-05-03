/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix extends Any with Equals with Linear { self =>
  override type Point = Matrix
  
  override type Vector = Matrix
  
  type Matrix <: basis.algebra.Matrix {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
    type Scalar = self.Scalar
  }
  
  type T <: basis.algebra.Matrix {
    type Matrix = self.T
    type T      = self.Matrix
    type Row    = self.Col
    type Col    = self.Row
    type Scalar = self.Scalar
  }
  
  type Row <: basis.algebra.Vector {
    type Vector = self.Row
    type Scalar = self.Scalar
  }
  
  type Col <: basis.algebra.Vector {
    type Vector = self.Col
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  def Matrix: basis.algebra.Matrix.Space {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
    type Scalar = self.Scalar
  }
  
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
      coords(j) = apply(n).asInstanceOf[AnyRef]
      j += 1
      n += 1
    }
    Matrix.Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def col(j: Int): Col = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("col "+ j)
    val coords = new Array[AnyRef](M)
    var i = 0
    var m = j
    while (i < coords.length) {
      coords(i) = apply(m).asInstanceOf[AnyRef]
      i += 1
      m += N
    }
    Matrix.Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
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
  
  def :⋅ (vector: Row): Col = {
    if (N != vector.N) throw new DimensionException
    val coords = new Array[AnyRef](M)
    var i = 0
    var i0 = 0
    while (i < coords.length) {
      var s = Matrix.Scalar.zero
      var n = i0
      var j = 0
      while (j < N) {
        s += apply(n) * vector.apply(j)
        n += 1
        j += 1
      }
      coords(i) = s.asInstanceOf[AnyRef]
      i += 1
      i0 += N
    }
    Matrix.Col(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def ⋅: (vector: Col): Row = {
    if (vector.N != M) throw new DimensionException
    val coords = new Array[AnyRef](N)
    var j = 0
    while (j < coords.length) {
      var s = Matrix.Scalar.zero
      var n = j
      var i = 0
      while (i < M) {
        s += vector.apply(i) * apply(n)
        n += N
        i += 1
      }
      coords(j) = s.asInstanceOf[AnyRef]
      j += 1
    }
    Matrix.Row(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = self.Scalar }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = self.Scalar })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = self.Scalar } = {
    if (N != that.M) throw new DimensionException
    val P = that.N
    val entries = new Array[AnyRef](M * P)
    var k = 0
    var i = 0
    var i0 = 0
    while (i < M) {
      var j = 0
      while (j < P) {
        var s = Matrix.Scalar.zero
        var m = i0
        var n = j
        var d = 0
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
    (that.Matrix.Row map Matrix.Col).apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def T: T = {
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
  
  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[basis.algebra.Matrix]
  
  override def equals(other: Any): Boolean = other match {
    case that: basis.algebra.Matrix =>
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
    import basis.util.MurmurHash._
    var dim = M * N
    var h = -1997372447
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

object Matrix {
  trait Space extends Ring.Scalar with Linear.Space { self =>
    override type Vector = Matrix
    
    type Matrix <: basis.algebra.Matrix {
      type Matrix = self.Matrix
      type T      = self.T
      type Row    = self.Row
      type Col    = self.Col
      type Scalar = self.Scalar
    }
    
    type T <: basis.algebra.Matrix {
      type Matrix = self.T
      type T      = self.Matrix
      type Row    = self.Col
      type Col    = self.Row
      type Scalar = self.Scalar
    }
    
    type Row <: basis.algebra.Vector {
      type Vector = self.Row
      type Scalar = self.Scalar
    }
    
    type Col <: basis.algebra.Vector {
      type Vector = self.Col
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    def T: basis.algebra.Matrix.Space {
      type Matrix = self.T
      type T      = self.Matrix
      type Row    = self.Col
      type Col    = self.Row
      type Scalar = self.Scalar
    }
    
    def Row: Vector.Space {
      type Vector = self.Row
      type Scalar = self.Scalar
    }
    
    def Col: Vector.Space {
      type Vector = self.Col
      type Scalar = self.Scalar
    }
    
    override def Scalar: Ring.Space {
      type Vector = self.Scalar
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
  }
}
