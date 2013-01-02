/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** An iterable sequence. Sequences traverse their elements in a definite order.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Iterating     2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
  * 
  * @define collection  sequence
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralSeqOps GeneralSeqOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictSeqOps StrictSeqOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictSeqOps NonStrictSeqOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Seq[+A] extends Any with Equals with Family[Seq[A]] with Container[A] {
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Quantifying */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of elements in this $collection.
    * @group Quantifying */
  def length: Int = {
    var n = 0
    var xs = iterator
    while (!xs.isEmpty) {
      n += 1
      xs.step()
    }
    n
  }
  
  /** Returns `true` if this $collection might equal another object, otherwise `false`.
    * @group Classifying */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Seq[_]]
  
  /** Returns `true` if this $collection equals another object, otherwise `false`.
    * @group Classifying */
  override def equals(other: Any): Boolean = other match {
    case that: Seq[_] if that.canEqual(this) =>
      val xs = this.iterator
      val ys = that.iterator
      while (!xs.isEmpty && !ys.isEmpty) {
        if (xs.head != ys.head) return false
        xs.step()
        ys.step()
      }
      xs.isEmpty && ys.isEmpty
    case _ => false
  }
  
  /** Returns a hash of the elements in this $collection.
    * @group Classifying */
  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var h = 63537721
    val xs = iterator
    while (!xs.isEmpty) {
      h = mix(h, xs.head.##)
      xs.step()
    }
    mash(h)
  }
}
