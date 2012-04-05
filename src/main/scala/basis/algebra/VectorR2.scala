/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A vector in a 2-dimensional `Real` vector space.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with two `Double` coordinates.
  * @param  x   The ''x''-coordinate.
  * @param  y   The ''y''-coordinate.
  * 
  * @define scalar  `Real` value
  */
final class VectorR2(val x: Double, val y: Double)
  extends RealVector[VectorR2] {
  
  def + (that: VectorR2): VectorR2 =
    new VectorR2(x + that.x, y + that.y)
  
  def unary_- : VectorR2 =
    new VectorR2(-x, -y)
  
  def - (that: VectorR2): VectorR2 =
    new VectorR2(x - that.x, y - that.y)
  
  def :* (scalar: Double): VectorR2 =
    new VectorR2(x * scalar, y * scalar)
  
  def *: (scalar: Double): VectorR2 =
    this :* scalar
  
  def / (scalar: Double): VectorR2 =
    new VectorR2(x / scalar, y / scalar)
  
  /** Returns the dot product of this $vector and another $vector. The name of
    * this method uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: VectorR2): Double = x * that.x + y * that.y
  
  /** Returns the length (euclidean norm) of this $vector. */
  def length: Double = math.sqrt(x * x + y * y)
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR2 => x == that.x && y == that.y
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(-1131959687, x), y))
  
  override def toString: String =
    "VectorR2"+"("+ x +", "+ y +")"
}

/** Contains factory methods for vectors in `R2`. */
object VectorR2 {
  /** The zero vector of `R2`. */
  val zero: VectorR2 = new VectorR2(0.0, 0.0)
  
  def apply(x: Double, y: Double): VectorR2 =
    new VectorR2(x, y)
  
  def unapply(vector: VectorR2): Some[(Double, Double)] =
    Some(vector.x, vector.y)
  
  /** The additive identity typeclass for `R2`. */
  implicit val additiveIdentity = new Zero(zero)
  
  /** The inner dot product typeclass for vectors in `R2`. */
  implicit val dotProduct = InnerProduct[VectorR2, Real]((u, v) => new Real(u ⋅ v))
  
  /** The euclidean norm typeclass for vectors in `R2`. */
  implicit val euclideanNorm = Norm[VectorR2, Real](u => new Real(u.length))
  
  /** The default struct for vectors in `R2`. */
  implicit lazy val struct = new StructVectorR2
  
  /** A struct for vectors in `R2`. */
  class StructVectorR2(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Double, Double, VectorR2](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Double] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Double] = field2
    
    def load(data: Data, address: Long): VectorR2 = {
      val x = data.loadDouble(address + offset1)
      val y = data.loadDouble(address + offset2)
      new VectorR2(x, y)
    }
    
    def store(data: Data, address: Long, vector: VectorR2) {
      data.storeDouble(address + offset1, vector.x)
      data.storeDouble(address + offset2, vector.y)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructVectorR2 =
      new StructVectorR2(offset1 + offset, size, alignment)
    
    override def toString: String = "StructVectorR2"
  }
}
