//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** An abstract 64-bit two's complement integer interval module.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Integral
  *
  * @define space   interval module
  */
trait ZIN extends FN {
  override type Vector <: VectorZIN

  override type Scalar = ZInterval

  override val Scalar: ZInterval.type

  implicit override def ScalarTag: scala.reflect.ClassTag[ZInterval] =
    scala.reflect.ClassTag(Predef.classOf[ZInterval])

  trait VectorZIN extends Any with VectorFN
}
