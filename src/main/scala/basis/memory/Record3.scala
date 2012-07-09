/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A value type for 3-ary product types and case classes.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * case class Vector3(x: Double, y: Double, z: Double)
  * implicit object Vector3 extends Record3[Double, Double, Double, Vector3]
  * }}}
  * 
  * @tparam T1        the instance type of the first column.
  * @tparam T2        the instance type of the second column.
  * @tparam T3        the instance type of the third column.
  * @tparam T         the modeled instance type.
  * @param  column1   Returns the value type of the first column.
  * @param  column2   Returns the value type of the second column.
  * @param  column3   Returns the value type of the third column.
  */
abstract class Record3[T1, T2, T3, T] private (
    protected val column1: ValType[T1],
    protected val column2: ValType[T2],
    protected val column3: ValType[T3],
    frameOffset: Int, frameSize: Int, frameAlignment: Int)
  extends Struct3[ValType[T1], ValType[T2], ValType[T3], T](
                  frameOffset, frameSize, frameAlignment)(
                  column1, column2, column3)
    with Framed[Record3[T1, T2, T3, T]] { self =>
  
  /** Constructs a value type with a specified frame.
    * 
    * @param  frameOffset     the preferred offset of the first column in the type's frame.
    * @param  frameSize       the preferred size of the type's frame.
    * @param  frameAlignment  the preferred alignment of the type's frame.
    * @param  column1         the implicit first column type.
    * @param  column2         the implicit second column type.
    * @param  column3         the implicit third column type.
    */
  def this(frameOffset: Int, frameSize: Int, frameAlignment: Int)
          (implicit column1: ValType[T1], column2: ValType[T2], column3: ValType[T3]) =
    this(column1, column2, column3, frameOffset, frameSize, frameAlignment)
  
  /** Constructs a value type with a minimal frame.
    * 
    * @param  column1   the implicit first column type.
    * @param  column2   the implicit second column type.
    * @param  column3   the implicit third column type.
    */
  def this()(implicit column1: ValType[T1], column2: ValType[T2], column3: ValType[T3]) =
    this(column1, column2, column3, 0, 0, 0)
  
  def apply(arg1: T1, arg2: T2, arg3: T3): T
  
  def unapply(value: T): Option[(T1, T2, T3)]
  
  /** Returns a value type that accesses just the first field. */
  def _1: Field1 = field1
  
  /** Returns a value type that accesses just the second field. */
  def _2: Field2 = field2
  
  /** Returns a value type that accesses just the third field. */
  def _3: Field3 = field3
  
  override def load(data: Data, address: Long): T = {
    val arg1 = field1.load(data, address)
    val arg2 = field2.load(data, address)
    val arg3 = field3.load(data, address)
    apply(arg1, arg2, arg3)
  }
  
  override def store(data: Data, address: Long, value: T) {
    val args = unapply(value).get
    field1.store(data, address, args._1)
    field2.store(data, address, args._2)
    field3.store(data, address, args._3)
  }
  
  override def framed(offset: Int, size: Int, alignment: Int): Frame =
    new Projection(offset, size, alignment)
  
  private final class Projection(frameOffset: Int, frameSize: Int, frameAlignment: Int)
    extends Record3[T1, T2, T3, T](
                    frameOffset, frameSize, frameAlignment)(
                    column1, column2, column3) {
    
    override def apply(arg1: T1, arg2: T2, arg3: T3): T = self.apply(arg1, arg2, arg3)
    
    override def unapply(value: T): Option[(T1, T2, T3)] = self.unapply(value)
    
    override def framed(offset: Int, size: Int, alignment: Int): Frame =
      new self.Projection(offset, size, alignment)
    
    override def toString: String = toString(self.toString, self.offset, self.size, self.alignment)
  }
}
