/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A composite value type with a single field. The wrapped column's value type
  * parameterizes the class, along with the modeled instance type. The class
  * constructor computes the structure's memory layout and makes available
  * to subclasses the calculated alignment, size, field offset, and field
  * projection. Subclasses use this information to load and store values.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * class Real(val value: Double)
  * 
  * // a basic struct implementation.
  * object Real extends Struct1[PaddedDouble, Real] {
  *   override def load(data: Data, address: Long): Real =
  *     new Real(field.load(data, address)) // specialized load from field projection.
  *   
  *   override def store(data: Data, address: Long, real: Real): Unit =
  *     field.store(data, address, real.value) // specialized store to field projection.
  * }
  * 
  * // a more advanced struct implementation.
  * class RealStruct(frameOffset: Int, frameSize: Int, frameAlignment: Int)
  *   extends Struct1[PaddedDouble, Real](frameOffset, frameSize, frameAlignment)
  *     with Framed[RealStruct] {
  *   
  *   def this() = this(0, 0, 0) // initializes a minimal frame.
  *   
  *   override def load(data: Data, address: Long): Real =
  *     new Real(data.loadDouble(address + offset))
  *   
  *   override def store(data: Data, address: Long, real: Real): Unit =
  *     data.storeDouble(address + offset, real.value)
  *   
  *   // this type acts as its own frame, making its use as a member
  *   // of another structure more efficient.
  *   override def framed(offset: Int, size: Int, alignment: Int): Frame =
  *     new RealStruct(offset, size, alignment)
  * }
  * }}}
  * 
  * @tparam F   the value type of the wrapped column.
  * @tparam T   the modeled instance type.
  */
abstract class Struct1[F <: Framed[F], T] private
    (column: F, frameOffset: Int, frameSize: Int, frameAlignment: Int)
  extends ValueType[T] {
  
  /** Constructs a value type with a specified frame.
    * 
    * @param  frameOffset     the preferred offset of the wrapped column in the type's frame.
    * @param  frameSize       the preferred size of the type's frame.
    * @param  frameAlignment  the preferred alignment of the type's frame.
    * @param  column          the implicit column type.
    */
  def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
          (implicit column: F) =
    this(column, frameOffset, frameSize, frameAlignment)
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column  the implicit column type.
    */
  def this()(implicit column: F) = this(column, 0, 0, 0)
  
  /** The frame type of the wrapped column. */
  protected type Field = F
  
  /** Returns the offset of the wrapped column in this type's frame. */
  override val offset: Int = align(column.alignment)(frameOffset)
  
  override val alignment: Int = align(column.alignment)(max(column.alignment, frameAlignment))
  
  override val size: Int = {
    val minimalSize = align(alignment)(offset + column.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** Returns the projection of the wrapped column into this type's frame. */
  protected val field: Field = column.framed(offset, size, alignment)
}
