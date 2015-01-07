//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A general 64-bit two's complement integer module.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Integral
  */
private[math] final class Z
    (override val dim: Int)
  extends AffineSpace with ZN {

  override type Point = Value

  override type Vector = Value

  override val Vector: this.type = this

  override type Scalar = Integer

  override val Scalar: Integer.type = Integer

  override def origin: Vector = zero

  override lazy val zero: Vector = super.zero

  override def apply(coords: Array[Long]): Vector = new Vector(coords)

  override def toString: String = "Z"+"("+ dim +")"

  final class Value(coords: Array[Long])
    extends PointElement
    with VectorZN {

    if (coords.length != Z.this.dim) throw new DimensionException

    override def dim: Int = coords.length

    override def apply(i: Int): Scalar = coords(i)
  }
}

/** A factory for general 64-bit two's complement integer modules.
  * @group Integral */
object Z {
  /** Returns a new 64-bit two's complement integer module with the given dimension. */
  def apply(dim: Int): ZN with AffineSpace = dim match {
    case 1 => Integer
    case 2 => Z2
    case 3 => Z3
    case _ => new Z(dim).asInstanceOf[ZN with AffineSpace] // cast avoids compiler crash
  }
}
