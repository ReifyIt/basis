/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A traversable collection of elements. Collection declares only a protected
  * `foreach` method; it has no public methods.
  * 
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  collection
  */
trait Collection[+A] extends Any with Family[Collection[A]] with Enumerator[A] {
  /** Returns a string representation of this $collection.
    * @group Classifying */
  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix)
    s.append('(')
    foreach(new Collection.AddString(s))
    s.append(')')
    s.toString
  }
  
  /** Returns a string that identifies this type of $collection.
    * @group Classifying */
  protected def stringPrefix: String = getClass.getSimpleName
}

private[collections] object Collection {
  import scala.runtime.AbstractFunction1
  
  final class AddString[-A](s: java.lang.StringBuilder) extends AbstractFunction1[A, Unit] {
    private[this] var e: Boolean = true
    override def apply(x: A): Unit = (if (e) { e = false; s } else s.append(", ")).append(x)
  }
}