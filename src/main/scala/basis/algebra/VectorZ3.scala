/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A vector in a 3-dimensional `Integer` module.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with three `Long` coordinates.
  * @param  x   The ''x''-coordinate.
  * @param  y   The ''y''-coordinate.
  * @param  z   The ''z''-coordinate.
  * 
  * @define scalar  `Integer` value
  */
final class VectorZ3(val x: Long, val y: Long, val z: Long)
  extends IntegerVector[VectorZ3] {
  
  def + (that: VectorZ3): VectorZ3 =
    new VectorZ3(x + that.x, y + that.y, z + that.z)
  
  def unary_- : VectorZ3 =
    new VectorZ3(-x, -y, -z)
  
  def - (that: VectorZ3): VectorZ3 =
    new VectorZ3(x - that.x, y - that.y, z - that.z)
  
  def :* (scalar: Long): VectorZ3 =
    new VectorZ3(x * scalar, y * scalar, z * scalar)
  
  def *: (scalar: Long): VectorZ3 =
    this :* scalar
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorZ3 => x == that.x && y == that.y && z == that.z
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(-1131959438, x), y), z))
  
  override def toString: String =
    "VectorZ3"+"("+ x +", "+ y +", "+ z +")"
}

/** Contains factory methods for vectors in `Z3`. */
object VectorZ3 {
  /** The zero vector of `Z3`. */
  val zero: VectorZ3 = new VectorZ3(0L, 0L, 0L)
  
  def apply(x: Long, y: Long, z: Long): VectorZ3 =
    new VectorZ3(x, y, z)
  
  def unapply(vector: VectorZ3): Some[(Long, Long, Long)] =
    Some(vector.x, vector.y, vector.z)
  
  /** The additive identity typeclass for `Z3`. */
  implicit val additiveIdentity = new Zero(zero)
  
  /** The default struct for vectors in `Z3`. */
  implicit lazy val struct = new StructVectorZ3
  
  /** A struct for vectors in `Z3`. */
  class StructVectorZ3(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct3[Long, Long, Long, VectorZ3](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Long] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Long] = field2
    
    /** The `z` field projection of this struct. */
    def z: Struct[Long] = field3
    
    def load(data: Data, address: Long): VectorZ3 = {
      val x = data.loadLong(address + offset1)
      val y = data.loadLong(address + offset2)
      val z = data.loadLong(address + offset3)
      new VectorZ3(x, y, z)
    }
    
    def store(data: Data, address: Long, vector: VectorZ3) {
      data.storeLong(address + offset1, vector.x)
      data.storeLong(address + offset2, vector.y)
      data.storeLong(address + offset3, vector.z)
    }
    
    override def project(offset: Long, alignment: Long, size: Long): StructVectorZ3 =
      new StructVectorZ3(offset1 + offset, alignment, size)
    
    override def toString: String = "StructVectorZ3"
  }
}
