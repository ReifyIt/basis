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
  * @group    Collections
  * 
  * @groupprio  Iterating     1
  * @groupprio  Traversing    2
  * @groupprio  Classifying   3
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
trait Seq[+A] extends Any with Equals with Family[Seq[_]] with Container[A] {
  /** Returns `true` if this $collection might equal another object, otherwise `false`.
    * @group Classifying */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Seq[_]]
  
  /** Returns `true` if this $collection contains the same elements in the same
    * order as another sequence, otherwise `false`.
    * @group Classifying */
  override def equals(other: Any): Boolean = other match {
    case that: Seq[_] =>
      (this.asInstanceOf[AnyRef] eq that.asInstanceOf[AnyRef]) ||
      (that canEqual this) && {
        val these = this.iterator
        val those = that.iterator
        while (!these.isEmpty && !those.isEmpty) {
          if (these.head != those.head) return false
          these.step()
          those.step()
        }
        these.isEmpty && those.isEmpty
      }
    case _ => false
  }
  
  /** Returns a hash of the elements in this $collection.
    * @group Classifying */
  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var h = seed[Seq[_]]
    val these = iterator
    while (!these.isEmpty) {
      h = mix(h, hash(these.head))
      these.step()
    }
    mash(h)
  }
}
