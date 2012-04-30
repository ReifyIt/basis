/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixSpace extends LinearSpace { self =>
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
  
  override type Scalar
  
  def T: MatrixSpace {
    type Matrix = self.T
    type T      = self.Matrix
    type Row    = self.Col
    type Col    = self.Row
    type Scalar = self.Scalar
  }
  
  def M: Int
  
  def N: Int
  
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
