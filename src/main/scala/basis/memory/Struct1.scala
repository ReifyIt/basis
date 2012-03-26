/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A template struct for unary product types.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * class Real(val value: Double)
  * 
  * // a basic struct implementation.
  * object Real extends Struct1[Double, Real] {
  *   def load(data: Data, address: Long): Real =
  *     new Real(field.load(data, address)) // specialized load from field projection.
  *   
  *   def store(data: Data, address: Long, real: Real): Unit =
  *     field.store(data, address, real.value) // specialized store to field projection.
  * }
  * 
  * // an alternate struct implementation.
  * class RealStruct(frameOffset: Long, frameSize: Long, frameAlignment: Long)
  *   extends Struct1[Double, Real](frameOffset, frameSize, frameAlignment) {
  *   
  *   def this() = this(0L, 0L, 0L) // constructs a minimal frame.
  *   
  *   def load(data: Data, address: Long): Real =
  *     new Real(data.loadDouble(address + offset))
  *   
  *   def store(data: Data, address: Long, real: Real): Unit =
  *     data.storeDouble(address + offset, real.value)
  *   
  *   override def project(offset: Long, size: Long, alignment: Long): RealStruct =
  *     new RealStruct(this.offset + offset, size, alignment) // make sure offset accumulates.
  * }
  * }}}
  * 
  * @constructor  Constructs a struct with a specified frame.
  * @tparam T1              the column type.
  * @tparam T               the type of this struct.
  * @param  frameOffset     the preferred offset of the column in this struct's frame.
  * @param  frameSize       the preferred size of this struct's frame.
  * @param  frameAlignment  the preferred alignment of this struct's frame.
  * @param  column          the column struct.
  */
abstract class Struct1[T1, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit protected val column: Struct[T1])
  extends Struct[T] {
  
  /** Constructs a struct with a minimal frame.
    * 
    * @param  column  the column struct.
    */
  def this()(implicit column: Struct[T1]) = this(0L, 0L, 0L)
  
  /** The offset of the column in this struct's frame. */
  protected val offset: Long = align(column.alignment)(frameOffset)
  
  val alignment: Long = align(column.alignment)(max(column.alignment, frameAlignment))
  
  val size: Long = {
    val minimalSize = align(alignment)(offset + column.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** The projection of the column into this struct's frame. */
  protected val field: Struct[T1] = column.project(offset, size, alignment)
  
  override def project(offset: Long, size: Long, alignment: Long): Struct[T] =
    new Frame(this.offset + offset, size, alignment)
}
