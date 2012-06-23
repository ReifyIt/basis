/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract space of ''N''x''N'' square matrices over a ring. 
  * 
  * @author Chris Sachs
  * 
  * @tparam V   The space of rows and columns.
  * @tparam S   The set of scalars.
  */
trait MatrixRing[V <: VectorSpace[S] with Singleton, S <: Ring with Singleton] extends Ring with MatrixSpace[V, V, S] {
  /** A matrix in this $space.
    * 
    * @define value   $matrix
    */
  trait Element extends Any with super[Ring].Element with super[MatrixSpace].Element {
    override protected def Matrix: MatrixRing.this.type = MatrixRing.this
    
    override def * (that: Matrix): Matrix = Matrix.product(this, that)
    
    def inverse: Option[Matrix]
    
    def det: Scalar
    
    def trace: Scalar
  }
  
  /** The type of elements in this $space; equivalent to the type of matrices. */
  override type Value = Matrix
  
  override type Matrix <: Element
  
  override type Transpose = this.type
  
  override def Transpose: this.type = this
  
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
