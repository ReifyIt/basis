/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

trait GeneralMatrix[M <: GeneralMatrix[M, T, C, R, S], 
                    T <: GeneralMatrix[T, M, R, C, S],
                    C <: CoordinateVector[C, S],
                    R <: CoordinateVector[R, S],
                    S <: RingElement[S]]
  extends Equals with GeneralVector[M, S] {
  
  def Space: MatrixModule {
    type Matrix = M
    type Transpose = T
    type ColumnVector = C
    type RowVector = R
    type Scalar = S
  }
  
  def entry(k: Int): S
  
  def entry(i: Int, j: Int): S = {
    if (i < 0 || i >= M || j < 0 || j >= N)
      throw new IndexOutOfBoundsException("row "+ i +", "+"column "+ j)
    entry(N * i + j)
  }
  
  def M: Int = Space.Column.dimension
  
  def N: Int = Space.Row.dimension
  
 def column(j: Int): C = {
    if (j < 0 || j >= N) throw new IndexOutOfBoundsException("column "+ j)
    val coords = new Array[AnyRef](M)
    var i = 0
    var m = j
    while (i < M) {
      coords(i) = entry(m)
      i += 1
      m += N
    }
    Space.Column(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def row(i: Int): R = {
    if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
    val coords = new Array[AnyRef](N)
    var j = 0
    var n = N * i
    while (j < N) {
      coords(j) = entry(n)
      j += 1
      n += 1
    }
    Space.Row(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def + (that: M): M = {
    if (M != that.M || N != that.N)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = entry(k) + that.entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[S]])
  }
  
  def unary_- : M = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = -entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[S]])
  }
  
  def - (that: M): M = {
    if (M != that.M || N != that.N)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = entry(k) - that.entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[S]])
  }
  
  def :* (scalar: S): M = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = entry(k) * scalar
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[S]])
  }
  
  def *: (scalar: S): M = {
    val entries = new Array[AnyRef](M * N)
    var k = 0
    while (k < entries.length) {
      entries(k) = scalar * entry(k)
      k += 1
    }
    Space(wrapRefArray(entries).asInstanceOf[Seq[S]])
  }
  
  def :* (vector: R): C = {
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
    Space.Column(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def *: (vector: C): R = {
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
    Space.Row(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def transpose: T = {
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
    Space.Transpose(wrapRefArray(entries).asInstanceOf[Seq[S]])
  }
  
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[GeneralMatrix[_, _, _, _, _]]
  
  override def equals(other: Any): Boolean = other match {
    case that: GeneralMatrix[_, _, _, _, _] =>
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
