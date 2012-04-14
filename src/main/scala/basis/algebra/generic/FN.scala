/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

trait FN extends LinearModule { self =>
  override type Vector <: VectorFN {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override def zero: Vector = {
    val z = Scalar.zero
    val coords = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = z
      i += 1
    }
    apply(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
  
  def dimension: Int
  
  def apply(coords: Seq[Scalar]): Vector
}
