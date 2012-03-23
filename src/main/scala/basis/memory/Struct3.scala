/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A typeclass that represents products of 3 value types.
  * 
  * Constructs and deconstructs instances with the abstract `apply` and
  * `unapply` methods, respectively. Easily turn case class companion objects
  * into value types by extending this class.
  * 
  * {{{
  * case class Vector3(x: Double, y: Double, z: Double)
  * 
  * implicit object Vector3 extends Struct3[Double, Double, Double, Vector3]
  * }}}
  * 
  * Override `load` and `store` to improve performance by eliminating boxing.
  * 
  * {{{
  * object Vector3 extends Struct3[Double, Double, Double, Vector3] {
  *   override def load(data: Data, address: Long): Vector3 = {
  *     val x = field1.load(data, address) // specialized load from field1 projection.
  *     val y = field2.load(data, address) // specialized load from field2 projection.
  *     val z = field3.load(data, address) // specialized load from field3 projection.
  *     apply(x, y, z)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector3) {
  *     field1.store(data, address, vector.x) // specialized store to field1 projection.
  *     field2.store(data, address, vector.y) // specialized store to field2 projection.
  *     field3.store(data, address, vector.z) // specialized store to field3 projection.
  *   }
  * }
  * }}}
  * 
  * Manually load and store fields if necessary.
  * 
  * {{{
  * object Vector3 extends Struct3[Double, Double, Double, Vector3] {
  *   override def load(data: Data, address: Long): Vector3 = {
  *     val x = data.loadDouble(address + offset1)
  *     val y = data.loadDouble(address + offset2)
  *     val z = data.loadDouble(address + offset3)
  *     apply(x, y, z)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector3) {
  *     data.storeDouble(address + offset1, vector.x)
  *     data.storeDouble(address + offset2, vector.y)
  *     data.storeDouble(address + offset3, vector.z)
  *   }
  * }
  * }}}
  * 
  * Implement a dedicated `Struct` class to optimize projections.
  * 
  * {{{
  * class Vector3Struct(frameOffset: Long, frameSize: Long, frameAlignment: Long)
  *   extends Struct3[Double, Double, Double, Vector3](frameOffset, frameSize, frameAlignment) {
  *   
  *   def this() = this(0L, 0L, 0L) // constructs a minimal frame.
  *   
  *   def apply(x: Double, y: Double, z: Double) = Vector3(x, y, z) // delegate to case class constructor
  *   
  *   def unapply(vector: Vector3) = Vector3.unapply(vector) // delegate to case class deconstructor
  *   
  *   override def load(data: Data, address: Long): Vector3 = {
  *     val x = field1.load(data, address)
  *     val y = field2.load(data, address)
  *     val z = field3.load(data, address)
  *     apply(x, y, z)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector3) {
  *     field1.store(data, address, vector.x)
  *     field2.store(data, address, vector.y)
  *     field3.store(data, address, vector.z)
  *   }
  *   
  *   override def project(offset: Long, size: Long, alignment: Long): Vector3Struct =
  *     new Vector3Struct(offset1 + offset, size, alignment) // make sure offset accumulates
  * }
  * }}}
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a value type with a given frame.
  * @tparam T1              the instance type of the first column.
  * @tparam T2              the instance type of the second column.
  * @tparam T3              the instance type of the third column.
  * @tparam T               the instance type of this value type.
  * @param  frameOffset     the cumulative offset of the first column into the frame.
  * @param  frameSize       the size of the frame.
  * @param  frameAlignment  the alignment of the frame.
  * @param  column1         the value type of the first column.
  * @param  column2         the value type of the second column.
  * @param  column3         the value type of the third column.
  */
abstract class Struct3[T1, T2, T3, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit val column1: Struct[T1], val column2: Struct[T2], val column3: Struct[T3])
  extends Struct[T] { struct =>
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column1   the value type of the first column.
    * @param  column2   the value type of the second column.
    * @param  column3   the value type of the third column.
    */
  def this()(implicit column1: Struct[T1], column2: Struct[T2], column3: Struct[T3]) = this(0L, 0L, 0L)
  
  def apply(_1: T1, _2: T2, _3: T3): T
  
  def unapply(value: T): Option[(T1, T2, T3)]
  
  /** The offset of the first column into this Struct's frame. */
  final val offset1: Long = align(column1.alignment)(frameOffset)
  
  /** The offset of the second column into this Struct's frame. */
  final val offset2: Long = align(column2.alignment)(offset1 + column1.size)
  
  /** The offset of the third column into this Struct's frame. */
  final val offset3: Long = align(column3.alignment)(offset2 + column2.size)
  
  final val alignment: Long = {
    val minimalAlignment = max(max(column1.alignment, column2.alignment), column3.alignment)
    align(minimalAlignment)(max(minimalAlignment, frameAlignment))
  }
  
  final val size: Long = {
    val minimalSize = align(alignment)(offset3 + column3.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** The projection of the first column into this Struct's frame. */
  final val field1: Struct[T1] = column1.project(offset1, size, alignment)
  
  /** The projection of the second column into this Struct's frame. */
  final val field2: Struct[T2] = column2.project(offset2, size, alignment)
  
  /** The projection of the third column into this Struct's frame. */
  final val field3: Struct[T3] = column3.project(offset3, size, alignment)
  
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
  
  override def project(offset: Long, size: Long, alignment: Long): Struct3[T1, T2, T3, T] =
    new Frame(offset1 + offset, size, alignment)
  
  protected class Frame(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct3[T1, T2, T3, T](frameOffset, frameSize, frameAlignment) {
    
    def apply(_1: T1, _2: T2, _3: T3): T = struct.apply(_1, _2, _3)
    
    def unapply(value: T): Option[(T1, T2, T3)] = struct.unapply(value)
    
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
