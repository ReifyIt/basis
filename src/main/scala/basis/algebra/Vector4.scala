/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A generic 4-dimensional vector over a ring.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with three scalar coordinates.
  * @tparam Scalar  the scalar type of the vector.
  * @param  x       The ''x''-coordinate.
  * @param  y       The ''y''-coordinate.
  * @param  z       The ''z''-coordinate.
  * @param  w       The ''w''-coordinate.
  */
final class Vector4[Scalar <: Ring[Scalar]]
    (val x: Scalar, val y: Scalar, val z: Scalar, val w: Scalar)
  extends Vector[Vector4[Scalar], Scalar] {
  
  def + (that: Vector4[Scalar]): Vector4[Scalar] =
    new Vector4[Scalar](x + that.x, y + that.y, z + that.z, w + that.w)
  
  def unary_- : Vector4[Scalar] =
    new Vector4[Scalar](-x, -y, -z, -w)
  
  def - (that: Vector4[Scalar]): Vector4[Scalar] =
    new Vector4[Scalar](x - that.x, y - that.y, z - that.z, w - that.w)
  
  def :* (scalar: Scalar): Vector4[Scalar] =
    new Vector4[Scalar](x * scalar, y * scalar, z * scalar, w * scalar)
  
  def *: (scalar: Scalar): Vector4[Scalar] =
    new Vector4[Scalar](scalar * x, scalar * y, scalar * z, scalar * w)
  
  /** Divides this $vector by a $scalar.
    * 
    * @param  scalar    the $scalar to divide by.
    * @param  isField   implicit evidence that `Scalar` is a `Field`.
    * @return the scaled $vector.
    */
  def / (scalar: Scalar)(implicit isField: Scalar <:< Field[Scalar]): Vector4[Scalar] =
    new Vector4[Scalar](x / scalar, y / scalar, z / scalar, w / scalar)
  
  /** Returns the dot product of this $vector and another $vector. The name of
    * this method uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: Vector4[Scalar]): Scalar =
    x * that.x + y * that.y + z * that.z + w * that.w
  
  /** Returns the euclidean norm of this $vector.
    * 
    * @param  isCompleteField   implicit evidence that `Scalar` is a `CompleteField`.
    * @return the square root of the dot product of this $vector with itself.
    */
  def norm(implicit isCompleteField: Scalar <:< CompleteField[Scalar]): Scalar =
    (x * x + y * y + z * z + w * w).sqrt
  
  override def equals(other: Any): Boolean = other match {
    case that: Vector4[_] => x.equals(that.x) && y.equals(that.y) && z.equals(that.z) && w.equals(that.w)
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(-867798853, x), y), z), w))
  
  override def toString: String =
    "Vector4"+"("+ x +", "+ y +", "+ z +", "+ w +")"
}

/** Contains factory methods for generic 4-dimensional vectors. */
object Vector4 {
  def apply[Scalar <: Ring[Scalar]](x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector4[Scalar] =
    new Vector4[Scalar](x, y, z, w)
  
  def unapply[Scalar <: Ring[Scalar]](vector: Vector4[Scalar]): Some[(Scalar, Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  /** Returns the additive identity typeclass for a kind of 4-dimensional vector. */
  implicit def additiveIdentity[Scalar <: Ring[Scalar] : AdditiveIdentity] =
    Zero(apply[Scalar](Zero, Zero, Zero, Zero))
  
  /** Returns the inner dot product typeclass for a kind of 4-dimensional vector. */
  implicit def dotProduct[Scalar <: Ring[Scalar]] =
    genericDotProduct.asInstanceOf[InnerProduct[Vector4[Scalar], Scalar]]
  
  private val genericDotProduct = InnerProduct[Vector4[Nothing], Ring[Nothing]] {
    (u, v) => (u ⋅ v).asInstanceOf[Ring[Nothing]]
  }
  
  /** Returns the euclidean norm typeclass for a kind of 4-dimensional vector. */
  implicit def euclideanNorm[Scalar <: CompleteField[Scalar]] =
    genericEuclideanNorm.asInstanceOf[Norm[Vector4[Scalar], Scalar]]
  
  private val genericEuclideanNorm = Norm[Vector4[Nothing], CompleteField[Nothing]] {
    u => u.norm.asInstanceOf[CompleteField[Nothing]]
  }
  
  /** Returns a default struct for a kind of 4-dimensional vector. */
  implicit def struct[Scalar <: Ring[Scalar] : Struct] = new StructVector4[Scalar]
  
  /** A struct for a kind of 4-dimensional vector. */
  class StructVector4[Scalar <: Ring[Scalar]]
      (frameOffset: Long, frameSize: Long, frameAlignment: Long)
      (implicit structScalar: Struct[Scalar])
    extends Struct4[Scalar, Scalar, Scalar, Scalar, Vector4[Scalar]](frameOffset, frameSize, frameAlignment) {
    
    def this()(implicit structScalar: Struct[Scalar]) = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Scalar] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Scalar] = field2
    
    /** The `z` field projection of this struct. */
    def z: Struct[Scalar] = field3
    
    /** The `w` field projection of this struct. */
    def w: Struct[Scalar] = field4
    
    def load(data: Data, address: Long): Vector4[Scalar] = {
      val x = field1.load(data, address)
      val y = field2.load(data, address)
      val z = field3.load(data, address)
      val w = field4.load(data, address)
      new Vector4[Scalar](x, y, z, w)
    }
    
    def store(data: Data, address: Long, vector: Vector4[Scalar]) {
      field1.store(data, address, vector.x)
      field2.store(data, address, vector.y)
      field3.store(data, address, vector.z)
      field4.store(data, address, vector.w)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructVector4[Scalar] =
      new StructVector4[Scalar](offset1 + offset, size, alignment)
    
    override def toString: String =
      "StructVector4"+"("+ structScalar +")"
  }
}
