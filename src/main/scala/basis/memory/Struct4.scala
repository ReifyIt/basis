/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A typeclass that represents products of 4 value types.
  * 
  * Constructs and deconstructs instances with the abstract `apply` and
  * `unapply` methods, respectively. Easily turn case class companion objects
  * into value types by extending this class.
  * 
  * {{{
  * case class Vector4(x: Double, y: Double, z: Double, w: Double)
  * 
  * implicit object Vector4 extends Struct4[Double, Double, Double, Double, Vector4]
  * }}}
  * 
  * Override `load` and `store` to improve performance by eliminating boxing.
  * 
  * {{{
  * object Vector4 extends Struct4[Double, Double, Double, Double, Vector4] {
  *   override def load(data: Data, address: Long): Vector4 = {
  *     val x = field1.load(data, address) // specialized load from field1 projection.
  *     val y = field2.load(data, address) // specialized load from field2 projection.
  *     val z = field3.load(data, address) // specialized load from field3 projection.
  *     val w = field4.load(data, address) // specialized load from field4 projection.
  *     apply(x, y, z, w)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector4) {
  *     field1.store(data, address, vector.x) // specialized store to field1 projection.
  *     field2.store(data, address, vector.y) // specialized store to field2 projection.
  *     field3.store(data, address, vector.z) // specialized store to field3 projection.
  *     field4.store(data, address, vector.w) // specialized store to field4 projection.
  *   }
  * }
  * }}}
  * 
  * Manually load and store fields if necessary.
  * 
  * {{{
  * object Vector4 extends Struct4[Double, Double, Double, Double, Vector4] {
  *   override def load(data: Data, address: Long): Vector4 = {
  *     val x = data.loadDouble(address + offset1)
  *     val y = data.loadDouble(address + offset2)
  *     val z = data.loadDouble(address + offset3)
  *     val w = data.loadDouble(address + offset4)
  *     apply(x, y, z, w)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector4) {
  *     data.storeDouble(address + offset1, vector.x)
  *     data.storeDouble(address + offset2, vector.y)
  *     data.storeDouble(address + offset3, vector.z)
  *     data.storeDouble(address + offset4, vector.w)
  *   }
  * }
  * }}}
  * 
  * Implement a dedicated `Struct` class to optimize projections.
  * 
  * {{{
  * class Vector4Struct(frameOffset: Long, frameSize: Long, frameAlignment: Long)
  *   extends Struct4[Double, Double, Double, Double, Vector4](frameOffset, frameSize, frameAlignment) {
  *   
  *   def this() = this(0L, 0L, 0L) // constructs a minimal frame.
  *   
  *   def apply(x: Double, y: Double, z: Double, w: Double) = Vector3(x, y, z, w) // delegate to case class constructor
  *   
  *   def unapply(vector: Vector4) = Vector4.unapply(vector) // delegate to case class deconstructor
  *   
  *   override def load(data: Data, address: Long): Vector4 = {
  *     val x = field1.load(data, address)
  *     val y = field2.load(data, address)
  *     val z = field3.load(data, address)
  *     val w = field4.load(data, address)
  *     apply(x, y, z, w)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector4) {
  *     field1.store(data, address, vector.x)
  *     field2.store(data, address, vector.y)
  *     field3.store(data, address, vector.z)
  *     field4.store(data, address, vector.w)
  *   }
  *   
  *   override def project(offset: Long, size: Long, alignment: Long): Vector4Struct =
  *     new Vector4Struct(offset1 + offset, size, alignment) // make sure offset accumulates
  * }
  * }}}
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a value type with a given frame.
  * @tparam T1              the Scala type of the first column.
  * @tparam T2              the Scala type of the second column.
  * @tparam T3              the Scala type of the third column.
  * @tparam T4              the Scala type of the fourth column.
  * @tparam T               the Scala type of this value type.
  * @param  frameOffset     the cumulative offset of the first column into the frame.
  * @param  frameSize       the size of the frame.
  * @param  frameAlignment  the alignment of the frame.
  * @param  column1         the value type of the first column.
  * @param  column2         the value type of the second column.
  * @param  column3         the value type of the third column.
  * @param  column4         the value type of the fourth column.
  */
abstract class Struct4[T1, T2, T3, T4, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit val column1: Struct[T1], val column2: Struct[T2], val column3: Struct[T3], val column4: Struct[T4])
  extends Struct[T] { struct =>
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column1   the value type of the first column.
    * @param  column2   the value type of the second column.
    * @param  column3   the value type of the third column.
    * @param  column4   the value type of the fourth column.
    */
  def this()(implicit column1: Struct[T1], column2: Struct[T2], column3: Struct[T3], column4: Struct[T4]) = this(0L, 0L, 0L)
  
  def apply(_1: T1, _2: T2, _3: T3, _4: T4): T
  
  def unapply(value: T): Option[(T1, T2, T3, T4)]
  
  /** The offset of the first column in this Struct's frame. */
  final val offset1: Long = align(column1.alignment)(frameOffset)
  
  /** The offset of the second column in this Struct's frame. */
  final val offset2: Long = align(column2.alignment)(offset1 + column1.size)
  
  /** The offset of the third column in this Struct's frame. */
  final val offset3: Long = align(column3.alignment)(offset2 + column2.size)
  
  /** The offset of the fourth column in this Struct's frame. */
  final val offset4: Long = align(column4.alignment)(offset3 + column3.size)
  
  final val alignment: Long = {
    val minimalAlignment = max(max(max(column1.alignment, column2.alignment), column3.alignment), column4.alignment)
    align(minimalAlignment)(max(minimalAlignment, frameAlignment))
  }
  
  final val size: Long = {
    val minimalSize = align(alignment)(offset4 + column4.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** The projection of the first column into this Struct's frame. */
  final val field1: Struct[T1] = column1.project(offset1, size, alignment)
  
  /** The projection of the second column into this Struct's frame. */
  final val field2: Struct[T2] = column2.project(offset2, size, alignment)
  
  /** The projection of the third column into this Struct's frame. */
  final val field3: Struct[T3] = column3.project(offset3, size, alignment)
  
  /** The projection of the fourth column into this Struct's frame. */
  final val field4: Struct[T4] = column4.project(offset4, size, alignment)
  
  def load(data: Data, address: Long): T = {
    val _1 = field1.load(data, address)
    val _2 = field2.load(data, address)
    val _3 = field3.load(data, address)
    val _4 = field4.load(data, address)
    apply(_1, _2, _3, _4)
  }
  
  def store(data: Data, address: Long, value: T) {
    val tuple = unapply(value).get
    field1.store(data, address, tuple._1)
    field2.store(data, address, tuple._2)
    field3.store(data, address, tuple._3)
    field4.store(data, address, tuple._4)
  }
  
  override def project(offset: Long, size: Long, alignment: Long): Struct4[T1, T2, T3, T4, T] =
    new Frame(offset1 + offset, size, alignment)
  
  protected class Frame(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct4[T1, T2, T3, T4, T](frameOffset, frameSize, frameAlignment) {
    
    def apply(_1: T1, _2: T2, _3: T3, _4: T4): T = struct.apply(_1, _2, _3, _4)
    
    def unapply(value: T): Option[(T1, T2, T3, T4)] = struct.unapply(value)
    
    override def load(data: Data, address: Long): T =
      struct.load(data, address + offset1)
    
    override def store(data: Data, address: Long, value: T): Unit =
      struct.store(data, address + offset1, value)
    
    override def project(offset: Long, size: Long, alignment: Long): Struct4[T1, T2, T3, T4, T] =
      new struct.Frame(offset1 + offset, size, alignment)
    
    override def toString: String =
      "%s.project(offset = %d, size = %d, alignment = %d)".format(struct, offset1, size, alignment)
  }
}
