/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

/** A traversable collection of elements. Collection declares only a protected
  * `foreach` method; it has no public methods.
  * 
  * @groupprio  Examining     -3
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
    var e = true
    s.append('(')
    foreach(x => (if (e) { e = false; s } else s.append(", ")).append(x))
    s.append(')')
    s.toString
  }
  
  /** Returns a string that identifies this type of $collection.
    * @group Classifying */
  protected def stringPrefix: String = getClass.getSimpleName
}
