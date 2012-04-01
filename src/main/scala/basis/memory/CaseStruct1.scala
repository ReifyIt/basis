/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A template struct for unary case classes.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * case class Real(value: Double)
  * implicit object Real extends CaseStruct1[Double, Real]
  * }}}
  * 
  * @constructor Constructs a struct with a specified frame.
  * @tparam T1              the column type.
  * @tparam T               the type of this struct.
  * @param  frameOffset     the preferred offset of the column in this struct's frame.
  * @param  frameSize       the preferred size of this struct's frame.
  * @param  frameAlignment  the preferred alignment of this struct's frame.
  * @param  column          the column struct.
  */
abstract class CaseStruct1[T1, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit column: Struct[T1])
  extends Struct1[T1, T](frameOffset, frameSize, frameAlignment) {
  
  /** Constructs a struct with a minimal frame.
    * 
    * @param  column  the column struct.
    */
  def this()(implicit column: Struct[T1]) = this(0L, 0L, 0L)
  
  def apply(_1: T1): T
  
  def unapply(value: T): Option[T1]
  
  def load(data: Data, address: Long): T =
    apply(field.load(data, address + offset))
  
  def store(data: Data, address: Long, value: T): Unit =
   field.store(data, address + offset, unapply(value).get)
}
