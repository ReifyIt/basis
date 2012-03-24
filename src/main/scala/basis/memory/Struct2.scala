/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A typeclass that represents products of 2 value types.
  * 
  * Constructs and deconstructs instances with the abstract `apply` and
  * `unapply` methods, respectively. Easily turn case class companion objects
  * into value types by extending this class.
  * 
  * {{{
  * case class Vector2(x: Double, y: Double)
  * 
  * implicit object Vector2 extends Struct2[Double, Double, Vector2]
  * }}}
  * 
  * Override `load` and `store` to improve performance by eliminating boxing.
  * 
  * {{{
  * object Vector2 extends Struct2[Double, Double, Vector2] {
  *   override def load(data: Data, address: Long): Vector2 = {
  *     val x = field1.load(data, address) // specialized load from field1 projection.
  *     val y = field2.load(data, address) // specialized load from field2 projection.
  *     apply(x, y)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector2) {
  *     field1.store(data, address, vector.x) // specialized store to field1 projection.
  *     field2.store(data, address, vector.y) // specialized store to field2 projection.
  *   }
  * }
  * }}}
  * 
  * Manually load and store fields if necessary.
  * 
  * {{{
  * object Vector2 extends Struct2[Double, Double, Vector2] {
  *   override def load(data: Data, address: Long): Vector2 = {
  *     val x = data.loadDouble(address + offset1)
  *     val y = data.loadDouble(address + offset2)
  *     apply(x, y)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector2) {
  *     data.storeDouble(address + offset1, vector.x)
  *     data.storeDouble(address + offset2, vector.y)
  *   }
  * }
  * }}}
  * 
  * Implement a dedicated `Struct` class to optimize projections.
  * 
  * {{{
  * class Vector2Struct(frameOffset: Long, frameSize: Long, frameAlignment: Long)
  *   extends Struct2[Double, Double, Vector2](frameOffset, frameSize, frameAlignment) {
  *   
  *   def this() = this(0L, 0L, 0L) // constructs a minimal frame.
  *   
  *   def apply(x: Double, y: Double) = Vector2(x, y) // delegate to case class constructor
  *   
  *   def unapply(vector: Vector2) = Vector2.unapply(vector) // delegate to case class deconstructor
  *   
  *   override def load(data: Data, address: Long): Vector2 = {
  *     val x = field1.load(data, address)
  *     val y = field2.load(data, address)
  *     apply(x, y)
  *   }
  *   
  *   override def store(data: Data, address: Long, vector: Vector2) {
  *     field1.store(data, address, vector.x)
  *     field2.store(data, address, vector.y)
  *   }
  *   
  *   override def project(offset: Long, size: Long, alignment: Long): Vector2Struct =
  *     new Vector2Struct(offset1 + offset, size, alignment) // make sure offset accumulates
  * }
  * }}}
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a value type with a given frame.
  * @tparam T1              the Scala type of the first column.
  * @tparam T2              the Scala type of the second column.
  * @tparam T               the Scala type of this value type.
  * @param  frameOffset     the cumulative offset of the first column into the frame.
  * @param  frameSize       the size of the frame.
  * @param  frameAlignment  the alignment of the frame.
  * @param  column1         the value type of the first column.
  * @param  column2         the value type of the second column.
  */
abstract class Struct2[T1, T2, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit val column1: Struct[T1], val column2: Struct[T2])
  extends Struct[T] { struct =>
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column1   the value type of the first column.
    * @param  column2   the value type of the second column.
    */
  def this()(implicit column1: Struct[T1], column2: Struct[T2]) = this(0L, 0L, 0L)
  
  def apply(_1: T1, _2: T2): T
  
  def unapply(value: T): Option[(T1, T2)]
  
  /** The offset of the first column in this Struct's frame. */
  final val offset1: Long = align(column1.alignment)(frameOffset)
  
  /** The offset of the second column in this Struct's frame. */
  final val offset2: Long = align(column2.alignment)(offset1 + column1.size)
  
  final val alignment: Long = {
    val minimalAlignment = max(column1.alignment, column2.alignment)
    align(minimalAlignment)(max(minimalAlignment, frameAlignment))
  }
  
  final val size: Long = {
    val minimalSize = align(alignment)(offset2 + column2.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** The projection of the first column in this Struct's frame. */
  final val field1: Struct[T1] = column1.project(offset1, size, alignment)
  
  /** The projection of the second column in this Struct's frame. */
  final val field2: Struct[T2] = column2.project(offset2, size, alignment)
  
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
  
  override def project(offset: Long, size: Long, alignment: Long): Struct2[T1, T2, T] =
    new Frame(offset1 + offset, size, alignment)
  
  protected class Frame(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[T1, T2, T](frameOffset, frameSize, frameAlignment) {
    
    def apply(_1: T1, _2: T2): T = struct.apply(_1, _2)
    
    def unapply(value: T): Option[(T1, T2)] = struct.unapply(value)
    
    override def load(data: Data, address: Long): T =
      struct.load(data, address + offset1)
    
    override def store(data: Data, address: Long, value: T): Unit =
      struct.store(data, address + offset1, value)
    
    override def project(offset: Long, size: Long, alignment: Long): Struct2[T1, T2, T] =
      new struct.Frame(offset1 + offset, size, alignment)
    
    override def toString: String =
      "%s.project(offset = %d, size = %d, alignment = %d)".format(struct, offset1, size, alignment)
  }
}
