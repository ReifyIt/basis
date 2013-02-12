/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A traversable collection.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Collections
  * 
  * @groupprio  Traversing    1
  * @groupprio  Classifying   2
  * 
  * @define collection  collection
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralCollectionOps GeneralCollectionOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictCollectionOps StrictCollectionOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictCollectionOps NonStrictCollectionOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Collection[+A] extends Any with Family[Collection[_]] with Enumerator[A] {
  /** Returns a string representation of this $collection.
    * @group Classifying */
  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix)
    s.append('(')
    traverse(new Collection.AddString(s))
    s.append(')')
    s.toString
  }
  
  /** Returns a string that identifies this family of $collection.
    * @group Classifying */
  protected def stringPrefix: String = getClass.getSimpleName
}

private[collections] object Collection {
  private[collections] final class AddString[-A](s: java.lang.StringBuilder)
    extends scala.runtime.AbstractFunction1[A, Unit] {
    private[this] var e: Boolean = true
    override def apply(x: A) {
      (if (e) { e = false; s } else s.append(", ")).append(x)
    }
  }
}
