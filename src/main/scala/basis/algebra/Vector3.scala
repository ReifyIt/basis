/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A generic 3-dimensional vector over a ring.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with three scalar coordinates.
  * @tparam Scalar  the scalar type of the vector.
  * @param  x       The ''x''-coordinate.
  * @param  y       The ''y''-coordinate.
  * @param  z       The ''z''-coordinate.
  */
final class Vector3[Scalar <: Ring[Scalar]]
    (val x: Scalar, val y: Scalar, val z: Scalar)
  extends Vector[Vector3[Scalar], Scalar] {
  
  def + (that: Vector3[Scalar]): Vector3[Scalar] =
    new Vector3[Scalar](x + that.x, y + that.y, z + that.z)
  
  def unary_- : Vector3[Scalar] =
    new Vector3[Scalar](-x, -y, -z)
  
  def - (that: Vector3[Scalar]): Vector3[Scalar] =
    new Vector3[Scalar](x - that.x, y - that.y, z - that.z)
  
  def :* (scalar: Scalar): Vector3[Scalar] =
    new Vector3[Scalar](x * scalar, y * scalar, z * scalar)
  
  def *: (scalar: Scalar): Vector3[Scalar] =
    new Vector3[Scalar](scalar * x, scalar * y, scalar * z)
  
  /** Divides this $vector by a $scalar.
    * 
    * @param  scalar    the $scalar to divide by.
    * @param  isField   implicit evidence that `Scalar` is a `Field`.
    * @return the scaled $vector.
    */
  def / (scalar: Scalar)(implicit isField: Scalar <:< Field[Scalar]): Vector3[Scalar] =
    new Vector3[Scalar](x / scalar, y / scalar, z / scalar)
  
  /** Returns the cross product of this $vector and another $vector. The name of
    * this method uses the unicode cross product operator U+2A2F.
    * 
    * @param  that  the $vector to take the cross product with.
    * @return the vector cross product of this $vector and the other $vector.
    */
  def ⨯ (that: Vector3[Scalar]): Vector3[Scalar] =
    new Vector3[Scalar](y * that.z + z * that.y, z * that.x + x * that.z, x * that.y + y * that.x)
  
  /** Returns the dot product of this $vector and another $vector. The name of
    * this method uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: Vector3[Scalar]): Scalar =
    x * that.x + y * that.y + z * that.z
  
  /** Returns the euclidean norm of this $vector.
    * 
    * @param  isCompleteField   implicit evidence that `Scalar` is a `CompleteField`.
    * @return the square root of the dot product of this $vector with itself.
    */
  def norm(implicit isCompleteField: Scalar <:< CompleteField[Scalar]): Scalar =
    (x * x + y * y + z * z).sqrt
  
  override def equals(other: Any): Boolean = other match {
    case that: Vector3[_] => x.equals(that.x) && y.equals(that.y) && z.equals(that.z)
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(-867798854, x), y), z))
  
  override def toString: String =
    "Vector3"+"("+ x +", "+ y +", "+ z +")"
}

/** Contains factory methods for generic 3-dimensional vectors. */
object Vector3 {
  def apply[Scalar <: Ring[Scalar]](x: Scalar, y: Scalar, z: Scalar): Vector3[Scalar] =
    new Vector3[Scalar](x, y, z)
  
  def unapply[Scalar <: Ring[Scalar]](vector: Vector3[Scalar]): Some[(Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z)
  
  /** Returns the additive identity of a kind of 3-dimensional vector. */
  implicit def additiveIdentity[Scalar <: Ring[Scalar] : AdditiveIdentity] =
    new AdditiveIdentity(new Vector3[Scalar](Zero, Zero, Zero))
  
  /** Returns a default struct for a kind of 3-dimensional vector. */
  implicit def struct[Scalar <: Ring[Scalar] : Struct] = new StructVector3[Scalar]
  
  /** A struct for a kind of 3-dimensional vector. */
  class StructVector3[Scalar <: Ring[Scalar]]
      (frameOffset: Long, frameSize: Long, frameAlignment: Long)
      (implicit structScalar: Struct[Scalar])
    extends Struct3[Scalar, Scalar, Scalar, Vector3[Scalar]](frameOffset, frameSize, frameAlignment) {
    
    def this()(implicit structScalar: Struct[Scalar]) = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Scalar] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Scalar] = field2
    
    /** The `z` field projection of this struct. */
    def z: Struct[Scalar] = field3
    
    def load(data: Data, address: Long): Vector3[Scalar] = {
      val x = field1.load(data, address)
      val y = field2.load(data, address)
      val z = field3.load(data, address)
      new Vector3[Scalar](x, y, z)
    }
    
    def store(data: Data, address: Long, vector: Vector3[Scalar]) {
      field1.store(data, address, vector.x)
      field2.store(data, address, vector.y)
      field3.store(data, address, vector.z)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructVector3[Scalar] =
      new StructVector3[Scalar](offset1 + offset, size, alignment)
    
    override def toString: String =
      "StructVector3"+"("+ structScalar +")"
  }
}
