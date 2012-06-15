/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixRing
    [V <: VectorSpace[S] with Singleton,
     W <: VectorSpace[S] with Singleton,
     S <: Ring with Singleton]
  extends Ring with MatrixSpace[V, W, S] {
  
  trait Element extends Any with super[Ring].Element with super[MatrixSpace].Element { this: Matrix =>
    override protected def Matrix: MatrixRing.this.type = MatrixRing.this
    
    def inverse: Option[Matrix]
    
    def det: Scalar
    
    def trace: Scalar
  }
  
  override type Matrix <: Element
  
  override val Transpose: MatrixRing[W, V, S]
  
  override def unit: Matrix = {
    val z = Scalar.zero.asInstanceOf[AnyRef]
    val u = Scalar.unit.asInstanceOf[AnyRef]
    val entries = new Array[AnyRef](M * N)
    var k = 0
    var i = 0
    var j = 0
    while (i < M) {
      while (j < N) {
        entries(i) = if (i != j) z else u
        k += 1
        j += 1
      }
      j = 0
      i += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
  }
}
