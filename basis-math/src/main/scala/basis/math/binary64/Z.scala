/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A general 64-bit two's complement integer module.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[math] final class Z
    (override val dim: Int)
  extends AffineSpace with ZN {
  
  final class Value(coords: Array[Long])
    extends super[AffineSpace].Value
       with super[ZN].Value {
    
    if (coords.length != Z.this.dim) throw new DimensionException
    
    override def dim: Int = coords.length
    
    override def apply(i: Int): Scalar = coords(i)
  }
  
  override type Point = Value
  
  override type Vector = Value
  
  override val Vector: this.type = this
  
  override type Scalar = Integer
  
  override val Scalar: Integer.type = Integer
  
  override def origin: Vector = zero
  
  override lazy val zero: Vector = super.zero
  
  override def apply(coords: Array[Long]): Vector = new Vector(coords)
  
  override def toString: String = "Z"+"("+ dim +")"
}

/** A factory for general 64-bit two's complement integer modules. */
object Z {
  /** Returns a new 64-bit two's complement integer module with the given dimension. */
  def apply(dim: Int): ZN with AffineSpace = dim match {
    case 1 => Integer
    case 2 => Z2
    case 3 => Z3
    case _ => new Z(dim).asInstanceOf[ZN with AffineSpace] // cast avoids compile crash
  }
}
