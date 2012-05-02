/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixRing extends Any with Ring with Matrix { self =>
  override type Matrix
  
  override type T = Matrix
  
  type Span
  
  override type Row = Span
  
  override type Col = Span
  
  override type Scalar
  
  override def Row: Vector.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  }
  
  override def Col: Vector.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  }
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Scalar
  
  override def apply(i: Int, j: Int): Scalar
  
  override def row(i: Int): Row
  
  override def col(j: Int): Col
  
  override def + (that: Matrix): Matrix
  
  override def unary_- : Matrix
  
  override def - (that: Matrix): Matrix
  
  override def :* (scalar: Scalar): Matrix
  
  override def *: (scalar: Scalar): Matrix
  
  override def :⋅ (vector: Row): Col
  
  override def ⋅: (vector: Col): Row
  
  override def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = self.Scalar }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = self.Scalar })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = self.Scalar }
  
  override def T: T
  
  override def * (that: Matrix): Matrix
  
  def inverse: Option[Matrix]
  
  def det: Scalar
  
  def trace: Scalar
}

object MatrixRing {
  trait Space extends Ring.Space with Matrix.Space { self =>
    override type Matrix <: MatrixRing {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override type T = Matrix
    
    type Span <: basis.algebra.Vector {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override type Row = Span
    
    override type Col = Span
    
    override type Scalar
    
    override def T: this.type = this
    
    override def M: Int
    
    override def N: Int
    
    override def zero: Matrix
    
    override def unit: Matrix
    
    override def apply(entries: TraversableOnce[Scalar]): Matrix
  }
  
  trait Template extends Any with Equals with Linear with Ring with Matrix.Template with MatrixRing { self =>
    override type Matrix <: MatrixRing {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override type Span <: basis.algebra.Vector {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    override def Matrix: MatrixRing.Space {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override def Row: Vector.Space {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override def Col: Vector.Space {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
  }
}
