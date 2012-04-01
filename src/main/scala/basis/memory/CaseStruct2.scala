/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A template struct for 2-ary case classes.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * case class Vector2(x: Double, y: Double)
  * implicit object Vector2 extends CaseStruct2[Double, Double, Vector2]
  * }}}
  * 
  * @constructor Constructs a struct with a specified frame.
  * @tparam T1              the first column type.
  * @tparam T2              the second column type.
  * @tparam T               the type of this struct.
  * @param  frameOffset     the preferred offset of the first column in this struct's frame.
  * @param  frameSize       the preferred size of this struct's frame.
  * @param  frameAlignment  the preferred alignment of this struct's frame.
  * @param  column1         the first column struct.
  * @param  column2         the second column struct.
  */
abstract class CaseStruct2[T1, T2, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit column1: Struct[T1], column2: Struct[T2])
  extends Struct2[T1, T2, T](frameOffset, frameSize, frameAlignment) {
  
  /** Constructs a struct with a minimal frame.
    * 
    * @param  column1   the first column struct.
    * @param  column2   the second column struct.
    */
  def this()(implicit column1: Struct[T1], column2: Struct[T2]) = this(0L, 0L, 0L)
  
  def apply(_1: T1, _2: T2): T
  
  def unapply(value: T): Option[(T1, T2)]
  
  def load(data: Data, address: Long): T = {
    val _1 = field1.load(data, address)
    val _2 = field2.load(data, address)
    apply(_1, _2)
  }
  
  def store(data: Data, address: Long, value: T) {
    val tuple = unapply(value).get
    field1.store(data, address, tuple._1)
    field2.store(data, address, tuple._2)
  }
}
