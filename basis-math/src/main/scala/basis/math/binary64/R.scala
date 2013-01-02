/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A general double-precision floating-point matrix space.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[math] final class R
    (override val dim: Int, RowSpace: RN, ColSpace: RN, TransposeSpace: RMxN)
  extends AffineSpace with RN with RMxN {
  
  def this(dim: Int) = this(dim, null, null, null)
  
  final class Value(entries: Array[Double])
    extends super[AffineSpace].Value
       with super[RN].Value
       with super[RMxN].Value {
    
    if (entries.length != R.this.dim) throw new DimensionException
    
    override def dim: Int = entries.length
    
    override def apply(k: Int): Scalar = entries(k)
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: this.type = this
  
  override type Matrix = Value
  
  override val Row: RN = if (RowSpace != null) RowSpace else this
  
  override val Col: RN = if (ColSpace != null) ColSpace else Real
  
  override val Transpose: RMxN {
    val Row: R.this.Col.type
    val Col: R.this.Row.type
  } = {
    if (TransposeSpace != null) TransposeSpace
    else new R(dim, Col, Row, this)
  }.asInstanceOf[RN with RMxN {
    val Row: R.this.Col.type
    val Col: R.this.Row.type
  }]
  
  override type Scalar = Real
  
  override val Scalar: Real.type = Real
  
  implicit override def ScalarTag: scala.reflect.ClassTag[Real] =
    scala.reflect.classTag[Real]
  
  override def origin: Matrix = zero
  
  override lazy val zero: Matrix = super.zero
  
  override def apply(entries: Array[Double]): Matrix = new Matrix(entries)
  
  override def apply(entries: Array[Real]): Matrix = super[RMxN].apply(entries)
  
  override def toString: String = {
    if ((Row eq this) && (Col eq Real)) "R"+"("+ dim +")"
    else "R"+"("+ Row +", "+ Col +")"
  }
}

/** A factory for general double-precision floating-point matrix and vector spaces. */
object R {
  /** Returns a new double-precision floating-point vector space with the given dimension. */
  def apply(dim: Int): RN with AffineSpace = dim match {
    case 1 => Real
    case 2 => R2
    case 3 => R3
    case 4 => R4
    case _ => new R(dim).asInstanceOf[RN with AffineSpace] // cast avoids compile crash
  }
  
  /** Returns a new double-precision floating-point matrix space. */
  def apply(RowSpace: RN, ColSpace: RN)
    : RMxN {
        val Row: RowSpace.type
        val Col: ColSpace.type
      } =
    new R(ColSpace.dim * RowSpace.dim, RowSpace, ColSpace, null).asInstanceOf[RMxN {
      val Row: RowSpace.type
      val Col: ColSpace.type
    }]
}
