/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A vector in a 2-dimensional `Integer` module.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with two `Long` coordinates.
  * @param  x   The ''x''-coordinate.
  * @param  y   The ''y''-coordinate.
  * 
  * @define scalar  `Integer` value
  */
final class VectorZ2(val x: Long, val y: Long)
  extends IntegerVector[VectorZ2] {
  
  def + (that: VectorZ2): VectorZ2 =
    new VectorZ2(x + that.x, y + that.y)
  
  def unary_- : VectorZ2 =
    new VectorZ2(-x, -y)
  
  def - (that: VectorZ2): VectorZ2 =
    new VectorZ2(x - that.x, y - that.y)
  
  def :* (scalar: Long): VectorZ2 =
    new VectorZ2(x * scalar, y * scalar)
  
  def *: (scalar: Long): VectorZ2 =
    this :* scalar
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorZ2 => x == that.x && y == that.y
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(-1131959439, x), y))
  
  override def toString: String =
    "VectorZ2"+"("+ x +", "+ y +")"
}

/** Contains factory methods for vectors in `Z2`. */
object VectorZ2 {
  /** The zero vector of `Z2`. */
  val zero: VectorZ2 = new VectorZ2(0L, 0L)
  
  def apply(x: Long, y: Long): VectorZ2 =
    new VectorZ2(x, y)
  
  def unapply(vector: VectorZ2): Some[(Long, Long)] =
    Some(vector.x, vector.y)
  
  /** The additive identity typeclass for `Z2`. */
  implicit val additiveIdentity = Zero(zero)
  
  /** The default struct for vectors in `Z2`. */
  implicit lazy val struct = new StructVectorZ2
  
  /** A struct for vectors in `Z2`. */
  class StructVectorZ2(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Long, Long, VectorZ2](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Long] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Long] = field2
    
    def load(data: Data, address: Long): VectorZ2 = {
      val x = data.loadLong(address + offset1)
      val y = data.loadLong(address + offset2)
      new VectorZ2(x, y)
    }
    
    def store(data: Data, address: Long, vector: VectorZ2) {
      data.storeLong(address + offset1, vector.x)
      data.storeLong(address + offset2, vector.y)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructVectorZ2 =
      new StructVectorZ2(offset1 + offset, size, alignment)
    
    override def toString: String = "StructVectorZ2"
  }
}
