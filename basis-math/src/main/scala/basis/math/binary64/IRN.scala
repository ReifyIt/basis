/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

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
trait IRN extends FN {
  trait Value extends super.Value
  
  override type Vector <: Value
  
  override type Scalar = RealInterval
  
  override val Scalar: RealInterval.type = RealInterval
  
  implicit override def ScalarTag: scala.reflect.ClassTag[RealInterval] =
    scala.reflect.ClassTag(Predef.classOf[RealInterval])
}
