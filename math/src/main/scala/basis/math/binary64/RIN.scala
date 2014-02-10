//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** An abstract double-precision floating-point interval vector space.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Real
  *
  * @define space   interval space
  */
trait RIN extends FN {
  override type Vector <: VectorRIN

  override type Scalar = RInterval

  override val Scalar: RInterval.type

  implicit override def ScalarTag: scala.reflect.ClassTag[RInterval] =
    scala.reflect.ClassTag(Predef.classOf[RInterval])

  trait VectorRIN extends Any with VectorFN
}
