/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import generic._

trait RMxN extends LinearSpace with FMxN { self =>
  override type Matrix <: MatrixRMxN {
    type Matrix       = self.Matrix
    type Transpose    = self.Transpose
    type RowVector    = self.RowVector
    type ColumnVector = self.ColumnVector
  }
  
  override type Transpose <: MatrixRMxN {
    type Matrix       = self.Transpose
    type Transpose    = self.Matrix
    type RowVector    = self.ColumnVector
    type ColumnVector = self.RowVector
  }
  
  override type RowVector <: VectorRN {
    type Vector = self.RowVector
  }
  
  override type ColumnVector <: VectorRN {
    type Vector = self.ColumnVector
  }
  
  override type Scalar = Real
  
  override def Scalar = Real
  
  override def Transpose: RMxN {
    type Matrix       = self.Transpose
    type Transpose    = self.Matrix
    type RowVector    = self.ColumnVector
    type ColumnVector = self.RowVector
    type Scalar       = self.Scalar
  }
  
  override def Row: RN {
    type Vector = self.RowVector
  }
  
  override def Column: RN {
    type Vector = self.ColumnVector
  }
  
  override def zero: Matrix = apply(new Array[Double](dimension))
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix = {
    val xs = entries.toSeq
    if (xs.length != dimension) throw new DimensionException
    // apply(xs.map(_.toDouble).toArray[Double]) // I know.
    val array = new Array[Double](dimension)
    var i = 0
    while (i < array.length) {
      array(i) = xs(i).toDouble
      i += 1
    }
    apply(array)
  }
  
  def apply(entries: Array[Double]): Matrix
}
