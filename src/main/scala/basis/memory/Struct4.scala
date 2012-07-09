/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A composite value type with four fields. The value types of the columns of
  * the structure parameterize the class, along with the modeled instance type.
  * The class constructor computes the structure's memory layout and makes
  * available to subclasses the calculated alignment, size, field offsets, and
  * field projections. Subclasses use this information to load and store values.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * class Vector4(val x: Double, val y: Double, val z: Double, val w: Double)
  * 
  * // a basic struct implementation.
  * object Vector4 extends Struct4[PaddedDouble, PaddedDouble, PaddedDouble, PaddedDouble, Vector4] {
  *   override def load(data: Data, address: Long): Vector4 = {
  *     // specialized loads from field projections.
  *     val x = field1.load(data, address)
  *     val y = field2.load(data, address)
  *     val z = field3.load(data, address)
  *     val w = field4.load(data, address)
  *     new Vector4(x, y, z, w)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector4) {
  *     // specialized stores to field projections.
  *     field1.store(data, address, vector.x)
  *     field2.store(data, address, vector.y)
  *     field3.store(data, address, vector.z)
  *     field4.store(data, address, vector.w)
  *   }
  * }
  * 
  * // a more advanced struct implementation.
  * class Vector4Struct(frameOffset: Int, frameSize: Int, frameAlignment: Int)
  *   extends Struct4[PaddedDouble, PaddedDouble, PaddedDouble, PaddedDouble, Vector4](
  *                   frameOffset, frameSize, frameAlignment)
  *     with Framed[Vector4Struct] {
  *   
  *   def this() = this(0, 0, 0) // initializes a minimal frame.
  *   
  *   // name and expose the field projections.
  *   def x: Field1 = field1
  *   def y: Field2 = field2
  *   def z: Field3 = field3
  *   def w: Field4 = field4
  *   
  *   override def load(data: Data, address: Long): Vector4 = {
  *     val x = field1.load(data, address)
  *     val y = field2.load(data, address)
  *     val z = field3.load(data, address)
  *     val w = field4.load(data, address)
  *     new Vector4(x, y, z, w)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector4) {
  *     field1.store(data, address, vector.x)
  *     field2.store(data, address, vector.y)
  *     field3.store(data, address, vector.z)
  *     field4.store(data, address, vector.w)
  *   }
  *   
  *   // this type acts as its own frame, making its use as a member
  *   // of another structure more efficient.
  *   override def framed(offset: Int, size: Int, alignment: Int): Frame =
  *     new Vector4Struct(offset, size, alignment)
  * }
  * }}}
  * 
  * @tparam F1  the value type of the first column.
  * @tparam F2  the value type of the second column.
  * @tparam F3  the value type of the third column.
  * @tparam F4  the value type of the fourth column.
  * @tparam T   the modeled instance type.
  */
abstract class Struct4[F1 <: Framed[F1], F2 <: Framed[F2], F3 <: Framed[F3], F4 <: Framed[F4], T] private (
    column1: F1, column2: F2, column3: F3, column4: F4,
    frameOffset: Int, frameSize: Int, frameAlignment: Int)
  extends ValType[T] {
  
  /** Constructs a value type with a specified frame.
    * 
    * @param  frameOffset     the preferred offset of the first column in the type's frame.
    * @param  frameSize       the preferred size of the type's frame.
    * @param  frameAlignment  the preferred alignment of the type's frame.
    * @param  column1         the implicit first column type.
    * @param  column2         the implicit second column type.
    * @param  column3         the implicit third column type.
    * @param  column4         the implicit fourth column type.
    */
  def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
          (implicit column1: F1, column2: F2, column3: F3, column4: F4) =
    this(column1, column2, column3, column4, frameOffset, frameSize, frameAlignment)
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column1   the implicit first column type.
    * @param  column2   the implicit second column type.
    * @param  column3   the implicit third column type.
    * @param  column4   the implicit fourth column type.
    */
  def this()(implicit column1: F1, column2: F2, column3: F3, column4: F4) =
    this(column1, column2, column3, column4, 0, 0, 0)
  
  /** The frame type of the first column. */
  protected type Field1 = F1
  
  /** The frame type of the second column. */
  protected type Field2 = F2
  
  /** The frame type of the third column. */
  protected type Field3 = F3
  
  /** The frame type of the fourth column. */
  protected type Field4 = F4
  
  /** Returns the offset of the first column in this type's frame. */
  protected val offset1: Int = align(column1.alignment)(frameOffset)
  
  /** Returns the offset of the second column in this type's frame. */
  protected val offset2: Int = align(column2.alignment)(offset1 + column1.size)
  
  /** Returns the offset of the third column in this type's frame. */
  protected val offset3: Int = align(column3.alignment)(offset2 + column2.size)
  
  /** Returns the offset of the fourth column in this type's frame. */
  protected val offset4: Int = align(column4.alignment)(offset3 + column3.size)
  
  override def offset: Int = offset1
  
  override val alignment: Int = {
    val minimalAlignment = max(max(max(column1.alignment, column2.alignment), column3.alignment), column4.alignment)
    align(minimalAlignment)(max(minimalAlignment, frameAlignment))
  }
  
  override val size: Int = {
    val minimalSize = align(alignment)(offset4 + column4.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** Returns the projection of the first column into this type's frame. */
  protected val field1: Field1 = column1.framed(offset1, size, alignment)
  
  /** Returns the projection of the second column into this type's frame. */
  protected val field2: Field2 = column2.framed(offset2, size, alignment)
  
  /** Returns the projection of the third column into this type's frame. */
  protected val field3: Field3 = column3.framed(offset3, size, alignment)
  
  /** Returns the projection of the fourth column into this type's frame. */
  protected val field4: Field4 = column4.framed(offset4, size, alignment)
}
