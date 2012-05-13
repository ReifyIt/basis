/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F3[S <: Ring with Singleton](val Scalar: S) extends Vector3Space[S] {
  final class Element(val x: Scalar, val y: Scalar, val z: Scalar) extends super.Element
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(x, y, z)
  
  override def toString: String = "F3"+"("+ Scalar +")"
}
