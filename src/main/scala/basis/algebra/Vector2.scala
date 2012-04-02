/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A generic 2-dimensional vector over a ring.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a vector with two scalar coordinates.
  * @tparam Scalar  the scalar type of the vector.
  * @param  x       The ''x''-coordinate.
  * @param  y       The ''y''-coordinate.
  */
final class Vector2[Scalar <: Ring[Scalar]]
    (val x: Scalar, val y: Scalar)
  extends Vector[Vector2[Scalar], Scalar] {
  
  def + (that: Vector2[Scalar]): Vector2[Scalar] =
    new Vector2[Scalar](x + that.x, y + that.y)
  
  def unary_- : Vector2[Scalar] =
    new Vector2[Scalar](-x, -y)
  
  def - (that: Vector2[Scalar]): Vector2[Scalar] =
    new Vector2[Scalar](x - that.x, y - that.y)
  
  def :* (scalar: Scalar): Vector2[Scalar] =
    new Vector2[Scalar](x * scalar, y * scalar)
  
  def *: (scalar: Scalar): Vector2[Scalar] =
    new Vector2[Scalar](scalar * x, scalar * y)
  
  /** Divides this $vector by a $scalar.
    * 
    * @param  scalar    the $scalar to divide by.
    * @param  isField   implicit evidence that `Scalar` is a `Field`.
    * @return the scaled $vector.
    */
  def / (scalar: Scalar)(implicit isField: Scalar <:< Field[Scalar]): Vector2[Scalar] =
    new Vector2[Scalar](x / scalar, y / scalar)
  
  /** Returns the dot product of this $vector and another $vector. The name of
    * this method uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: Vector2[Scalar]): Scalar =
    x * that.x + y * that.y
  
  /** Returns the euclidean norm of this $vector.
    * 
    * @param  isCompleteField   implicit evidence that `Scalar` is a `CompleteField`.
    * @return the square root of the dot product of this $vector with itself.
    */
  def norm(implicit isCompleteField: Scalar <:< CompleteField[Scalar]): Scalar =
    (x * x + y * y).sqrt
  
  override def equals(other: Any): Boolean = other match {
    case that: Vector2[_] => x.equals(that.x) && y.equals(that.y)
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(-867798855, x), y))
  
  override def toString: String =
    "Vector2"+"("+ x +", "+ y +")"
}

/** Contains factory methods for generic 2-dimensional vectors. */
object Vector2 {
  def apply[Scalar <: Ring[Scalar]](x: Scalar, y: Scalar): Vector2[Scalar] =
    new Vector2[Scalar](x, y)
  
  def unapply[Scalar <: Ring[Scalar]](vector: Vector2[Scalar]): Some[(Scalar, Scalar)] =
    Some(vector.x, vector.y)
  
  /** Returns the additive identity of a kind of 2-dimensional vector. */
  implicit def additiveIdentity[Scalar <: Ring[Scalar] : AdditiveIdentity] =
    new AdditiveIdentity(new Vector2[Scalar](Zero, Zero))
  
  /** Returns a default struct for a kind of 2-dimensional vector. */
  implicit def struct[Scalar <: Ring[Scalar] : Struct] = new StructVector2[Scalar]
  
  /** A struct for a kind of 2-dimensional vector. */
  class StructVector2[Scalar <: Ring[Scalar]]
      (frameOffset: Long, frameSize: Long, frameAlignment: Long)
      (implicit structScalar: Struct[Scalar])
    extends Struct2[Scalar, Scalar, Vector2[Scalar]](frameOffset, frameSize, frameAlignment) {
    
    def this()(implicit structScalar: Struct[Scalar]) = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Scalar] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Scalar] = field2
    
    def load(data: Data, address: Long): Vector2[Scalar] = {
      val x = field1.load(data, address)
      val y = field2.load(data, address)
      new Vector2[Scalar](x, y)
    }
    
    def store(data: Data, address: Long, vector: Vector2[Scalar]) {
      field1.store(data, address, vector.x)
      field2.store(data, address, vector.y)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructVector2[Scalar] =
      new StructVector2[Scalar](offset1 + offset, size, alignment)
    
    override def toString: String =
      "StructVector2"+"("+ structScalar +")"
  }
}
