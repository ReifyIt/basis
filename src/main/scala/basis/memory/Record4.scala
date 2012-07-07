/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A value type for 4-ary product types and case classes.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * case class Vector4(x: Double, y: Double, z: Double, w: Double)
  * implicit object Vector4 extends Record4[Double, Double, Double, Double, Vector4]
  * }}}
  * 
  * @tparam T1        the instance type of the first column.
  * @tparam T2        the instance type of the second column.
  * @tparam T3        the instance type of the third column.
  * @tparam T4        the instance type of the fourth column.
  * @tparam T         the modeled instance type.
  * @param  column1   Returns the value type of the first column.
  * @param  column2   Returns the value type of the second column.
  * @param  column3   Returns the value type of the third column.
  * @param  column4   Returns the value type of the fourth column.
  */
abstract class Record4[T1, T2, T3, T4, T] private (
    protected val column1: ValueType[T1],
    protected val column2: ValueType[T2],
    protected val column3: ValueType[T3],
    protected val column4: ValueType[T4],
    frameOffset: Int, frameSize: Int, frameAlignment: Int)
  extends Struct4[ValueType[T1], ValueType[T2], ValueType[T3], ValueType[T4], T](
                  frameOffset, frameSize, frameAlignment)(
                  column1, column2, column3, column4)
    with Framed[Record4[T1, T2, T3, T4, T]] { self =>
  
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
          (implicit column1: ValueType[T1], column2: ValueType[T2], column3: ValueType[T3], column4: ValueType[T4]) =
    this(column1, column2, column3, column4, frameOffset, frameSize, frameAlignment)
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column1   the implicit first column type.
    * @param  column2   the implicit second column type.
    * @param  column3   the implicit third column type.
    * @param  column4   the implicit fourth column type.
    */
  def this()(implicit column1: ValueType[T1], column2: ValueType[T2], column3: ValueType[T3], column4: ValueType[T4]) =
    this(column1, column2, column3, column4, 0, 0, 0)
  
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4): T
  
  def unapply(value: T): Option[(T1, T2, T3, T4)]
  
  /** Returns a value type that accesses just the first field. */
  def _1: Field1 = field1
  
  /** Returns a value type that accesses just the first field. */
  def _2: Field2 = field2
  
  /** Returns a value type that accesses just the first field. */
  def _3: Field3 = field3
  
  /** Returns a value type that accesses just the first field. */
  def _4: Field4 = field4
  
  override def load(data: Data, address: Long): T = {
    val arg1 = field1.load(data, address)
    val arg2 = field2.load(data, address)
    val arg3 = field3.load(data, address)
    val arg4 = field4.load(data, address)
    apply(arg1, arg2, arg3, arg4)
  }
  
  override def store(data: Data, address: Long, value: T) {
    val args = unapply(value).get
    field1.store(data, address, args._1)
    field2.store(data, address, args._2)
    field3.store(data, address, args._3)
    field4.store(data, address, args._4)
  }
  
  override def framed(offset: Int, size: Int, alignment: Int): Frame =
    new Projection(offset, size, alignment)
  
  private final class Projection(frameOffset: Int, frameSize: Int, frameAlignment: Int)
    extends Record4[T1, T2, T3, T4, T](
                    frameOffset, frameSize, frameAlignment)(
                    column1, column2, column3, column4) {
    
    override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4): T = self.apply(arg1, arg2, arg3, arg4)
    
    override def unapply(value: T): Option[(T1, T2, T3, T4)] = self.unapply(value)
    
    override def framed(offset: Int, size: Int, alignment: Int): Frame =
      new self.Projection(offset, size, alignment)
    
    override def toString: String = toString(self.toString, self.offset, self.size, self.alignment)
  }
}
