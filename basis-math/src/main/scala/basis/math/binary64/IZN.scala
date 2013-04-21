/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

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
trait IZN extends FN {
  trait Value extends super.Value
  
  override type Vector <: Value
  
  override type Scalar = IntegerInterval
  
  override val Scalar: IntegerInterval.type = IntegerInterval
  
  implicit override def ScalarTag: scala.reflect.ClassTag[IntegerInterval] =
    scala.reflect.ClassTag(Predef.classOf[IntegerInterval])
}
