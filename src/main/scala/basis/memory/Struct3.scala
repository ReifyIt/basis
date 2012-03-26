/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A template struct for 3-ary product types.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * class Vector3(val x: Double, val y: Double, val z: Double)
  * 
  * // a basic struct implementation.
  * object Vector3 extends Struct3[Double, Double, Double, Vector3] {
  *   def load(data: Data, address: Long): Vector3 = {
  *     val x = field1.load(data, address) // specialized load from field1 projection.
  *     val y = field2.load(data, address) // specialized load from field2 projection.
  *     val z = field3.load(data, address) // specialized load from field3 projection.
  *     new Vector3(x, y, z)
  *   }
  *   
  *   def store(data: Data, address: Long, vector: Vector3) {
  *     field1.store(data, address, vector.x) // specialized store to field1 projection.
  *     field2.store(data, address, vector.y) // specialized store to field2 projection.
  *     field3.store(data, address, vector.z) // specialized store to field3 projection.
  *   }
  * }
  * 
  * // an alternate struct implementation.
  * class Vector3Struct(frameOffset: Long, frameSize: Long, frameAlignment: Long)
  *   extends Struct3[Double, Double, Double, Vector3](frameOffset, frameSize, frameAlignment) {
  *   
  *   def this() = this(0L, 0L, 0L) // constructs a minimal frame.
  *   
  *   def load(data: Data, address: Long): Vector3 = {
  *     val x = data.loadDouble(address + offset1)
  *     val y = data.loadDouble(address + offset2)
  *     val z = data.loadDouble(address + offset3)
  *     new Vector3(x, y, z)
  *   }
  *   
  *   def store(data: Data, address: Long, vector: Vector3) {
  *     data.storeDouble(address + offset1, vector.x)
  *     data.storeDouble(address + offset2, vector.y)
  *     data.storeDouble(address + offset3, vector.z)
  *   }
  *   
  *   override def project(offset: Long, size: Long, alignment: Long): Vector3Struct =
  *     new Vector3Struct(offset1 + offset, size, alignment) // make sure offset accumulates.
  * }
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
abstract class Struct3[T1, T2, T3, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit protected val column1: Struct[T1], protected val column2: Struct[T2], protected val column3: Struct[T3])
  extends Struct[T] { struct =>
  
  /** Constructs a struct with a minimal frame.
    * 
    * @param  column1   the first column struct.
    * @param  column2   the second column struct.
    * @param  column3   the third column struct.
    */
  def this()(implicit column1: Struct[T1], column2: Struct[T2], column3: Struct[T3]) = this(0L, 0L, 0L)
  
  /** The offset of the first column in this struct's frame. */
  protected val offset1: Long = align(column1.alignment)(frameOffset)
  
  /** The offset of the second column in this struct'ss frame. */
  protected val offset2: Long = align(column2.alignment)(offset1 + column1.size)
  
  /** The offset of the third column in this struct's frame. */
  protected val offset3: Long = align(column3.alignment)(offset2 + column2.size)
  
  val alignment: Long = {
    val minimalAlignment = max(max(column1.alignment, column2.alignment), column3.alignment)
    align(minimalAlignment)(max(minimalAlignment, frameAlignment))
  }
  
  val size: Long = {
    val minimalSize = align(alignment)(offset3 + column3.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** The projection of the first column into this struct's frame. */
  protected val field1: Struct[T1] = column1.project(offset1, size, alignment)
  
  /** The projection of the second column into this struct's frame. */
  protected val field2: Struct[T2] = column2.project(offset2, size, alignment)
  
  /** The projection of the third column into this struct's frame. */
  protected val field3: Struct[T3] = column3.project(offset3, size, alignment)
  
  override def project(offset: Long, size: Long, alignment: Long): Struct3[T1, T2, T3, T] =
    new Frame(offset1 + offset, size, alignment)
  
  protected class Frame(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct3[T1, T2, T3, T](frameOffset, frameSize, frameAlignment) {
    
    override def load(data: Data, address: Long): T =
      struct.load(data, address + offset1)
    
    override def store(data: Data, address: Long, value: T): Unit =
      struct.store(data, address + offset1, value)
    
    override def project(offset: Long, size: Long, alignment: Long): Struct3[T1, T2, T3, T] =
      new struct.Frame(offset1 + offset, size, alignment)
    
    override def toString: String =
      "%s.project(offset = %d, size = %d, alignment = %d)".format(struct, offset1, size, alignment)
  }
}
