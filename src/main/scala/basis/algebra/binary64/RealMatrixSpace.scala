/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RealMatrixSpace extends MatrixSpace { self =>
  override type Matrix <: RealMatrix {
    type Matrix = self.Matrix
    type T      = self.T
    type Row    = self.Row
    type Col    = self.Col
  }
  
  override type T <: RealMatrix {
    type Matrix = self.T
    type T      = self.Matrix
    type Row    = self.Col
    type Col    = self.Row
  }
  
  override type Row <: RealVector {
    type Vector = self.Row
    type Scalar = self.Scalar
  }
  
  override type Col <: RealVector {
    type Vector = self.Col
    type Scalar = self.Scalar
  }
  
  override type Scalar = Real
  
  override def T: RealMatrixSpace {
    type Matrix = self.T
    type T      = self.Matrix
    type Row    = self.Col
    type Col    = self.Row
  }
  
  override def M: Int
  
  override def N: Int
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix
  
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
}
