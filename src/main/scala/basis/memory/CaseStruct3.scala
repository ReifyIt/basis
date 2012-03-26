/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A template struct for 3-ary case classes.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * case class Vector3(x: Double, y: Double, z: Double)
  * implicit object Vector3 extends CaseStruct3[Double, Double, Double, Vector3]
  * }}}
  * 
  * @constructor  Constructs a struct with a specified frame.
  * @tparam T1              the first column type.
  * @tparam T2              the second column type.
  * @tparam T3              the third column type.
  * @tparam T               the type of this struct.
  * @param  frameOffset     the preferred offset of the first column in this struct's frame.
  * @param  frameSize       the preferred size of this struct's frame.
  * @param  frameAlignment  the preferred alignment of this struct's frame.
  * @param  column1         the first column struct.
  * @param  column2         the second column struct.
  * @param  column3         the third column struct.
  */
abstract class CaseStruct3[T1, T2, T3, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit column1: Struct[T1], column2: Struct[T2], column3: Struct[T3])
  extends Struct3[T1, T2, T3, T](frameOffset, frameSize, frameAlignment) {
  
  /** Constructs a struct with a minimal frame.
    * 
    * @param  column1         the first column struct.
    * @param  column2         the second column struct.
    * @param  column3         the third column struct.
    */
  def this()(implicit column1: Struct[T1], column2: Struct[T2], column3: Struct[T3]) = this(0L, 0L, 0L)
  
  def apply(_1: T1, _2: T2, _3: T3): T
  
  def unapply(value: T): Option[(T1, T2, T3)]
  
  def load(data: Data, address: Long): T = {
    val _1 = field1.load(data, address)
    val _2 = field2.load(data, address)
    val _3 = field3.load(data, address)
    apply(_1, _2, _3)
  }
  
  def store(data: Data, address: Long, value: T) {
    val tuple = unapply(value).get
    field1.store(data, address, tuple._1)
    field2.store(data, address, tuple._2)
    field3.store(data, address, tuple._3)
  }
}
