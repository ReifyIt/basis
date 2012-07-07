/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A value type for unary product types and case classes.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * case class Real(value: Double)
  * implicit object Real extends Record1[Double, Real]
  * }}}
  * 
  * @tparam T1      the instance type of the wrapped column.
  * @tparam T       the modeled instance type.
  * @param  column  Returns the value type of the wrapped column.
  */
abstract class Record1[T1, T] private (
    protected val column: ValueType[T1],
    frameOffset: Int, frameSize: Int, frameAlignment: Int)
  extends Struct1[ValueType[T1], T](frameOffset, frameSize, frameAlignment)(column)
    with Framed[Record1[T1, T]] { self =>
  
  /** Constructs a value type with a specified frame.
    * 
    * @param  frameOffset     the preferred offset of the column in the type's frame.
    * @param  frameSize       the preferred size of the type's frame.
    * @param  frameAlignment  the preferred alignment of the type's frame.
    * @param  column          the implicit column type.
    */
  def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
          (implicit column: ValueType[T1]) =
    this(column, frameOffset, frameSize, frameAlignment)
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column  the implicit column type.
    */
  def this()(implicit column: ValueType[T1]) = this(column, 0, 0, 0)
  
  def apply(arg1: T1): T
  
  def unapply(value: T): Option[T1]
  
  override def load(data: Data, address: Long): T =
    apply(field.load(data, address + offset))
  
  override def store(data: Data, address: Long, value: T): Unit =
    field.store(data, address + offset, unapply(value).get)
  
  override def framed(offset: Int, size: Int, alignment: Int): Frame =
    new Projection(offset, size, alignment)
  
  private final class Projection(frameOffset: Int, frameSize: Int, frameAlignment: Int)
    extends Record1[T1, T](frameOffset, frameSize, frameAlignment)(column) {
    
    override def apply(arg1: T1): T = self.apply(arg1)
    
    override def unapply(value: T): Option[T1] = self.unapply(value)
    
    override def framed(offset: Int, size: Int, alignment: Int): Frame =
      new self.Projection(offset, size, alignment)
    
    override def toString: String = toString(self.toString, self.offset, self.size, self.alignment)
  }
}
