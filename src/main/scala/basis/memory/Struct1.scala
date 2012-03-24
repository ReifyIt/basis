/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.math.max

/** A typeclass that maps a value type to a different Scala type.
  * 
  * Constructs and deconstructs instances with the abstract `apply` and
  * `unapply` methods. Easily turn wrapper case class companion objects
  * into value types by extending this class.
  * 
  * {{{
  * case class Real(value: Double)
  * 
  * implicit object Real extends Struct1[Double, Real]
  * }}}
  * 
  * Override `load` and `store` to improve performance by eliminating boxing.
  * 
  * {{{
  * object Real extends Struct1[Double, Real] {
  *   override def load(data: Data, address: Long): Real = {
  *     val value = field.load(data, address) // specialized load from field projection.
  *     apply(value)
  *   }
  *   
  *   override def store(data: Data, address: Long, real: Real) {
  *     field.store(data, address, real.value) // specialized store to field projection.
  *   }
  * }
  * }}}
  * 
  * Manually load and store fields if necessary.
  * 
  * {{{
  * object Real extends Struct1[Double, Real] {
  *   override def load(data: Data, address: Long): Real = {
  *     val value = data.loadDouble(address + offset)
  *     apply(value)
  *   }
  *   
  *   override def store(data: Data, address: Long, real: Real) {
  *     data.storeDouble(address + offset, real.value)
  *   }
  * }
  * }}}
  * 
  * Implement a dedicated `Struct` class to optimize projections.
  * 
  * {{{
  * class RealStruct(frameOffset: Long, frameSize: Long, frameAlignment: Long)
  *   extends Struct1[Double, Real](frameOffset, frameSize, frameAlignment) {
  *   
  *   def this() = this(0L, 0L, 0L) // constructs a minimal frame.
  *   
  *   def apply(value: Double) = Real(value) // delegate to case class constructor
  *   
  *   def unapply(real: Real) = Real.unapply(real) // delegate to case class deconstructor
  *   
  *   override def load(data: Data, address: Long): Real = {
  *     val value = field.load(data, address)
  *     apply(value)
  *   }
  *   
  *   override def store(data: Data, address: Long, real: Real) {
  *     field.store(data, address, real.value)
  *   }
  *   
  *   override def project(offset: Long, size: Long, alignment: Long): RealStruct =
  *     new RealStruct(this.offset + offset, size, alignment) // make sure offset accumulates
  * }
  * }}}
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a value type with a given frame.
  * @tparam T1              the Scala type of the mapped value type.
  * @tparam T               the Scala type of this value type.
  * @param  frameOffset     the cumulative offset into the frame.
  * @param  frameSize       the size of the frame.
  * @param  frameAlignment  the alignment of the frame.
  * @param  column          the mapped value type.
  */
abstract class Struct1[T1, T]
    (frameOffset: Long, frameSize: Long, frameAlignment: Long)
    (implicit val column: Struct[T1])
  extends Struct[T] {
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column  the mapped value type.
    */
  def this()(implicit column: Struct[T1]) = this(0L, 0L, 0L)
  
  def apply(_1: T1): T
  
  def unapply(value: T): Option[T1]
  
  /** The offset of the column in this Struct's frame. */
  final val offset: Long = align(column.alignment)(frameOffset)
  
  final val alignment: Long = align(column.alignment)(max(column.alignment, frameAlignment))
  
  final val size: Long = {
    val minimalSize = align(alignment)(offset + column.size)
    align(alignment)(max(minimalSize, frameSize))
  }
  
  /** The projection of the column in this Struct's frame. */
  final val field: Struct[T1] = column.project(offset, size, alignment)
  
  def load(data: Data, address: Long): T = {
    val _1 = field.load(data, address)
    apply(_1)
  }
  
  def store(data: Data, address: Long, value: T) {
    val it = unapply(value).get
    field.store(data, address, it)
  }
}
