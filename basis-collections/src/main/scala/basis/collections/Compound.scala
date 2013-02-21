/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A collection of disjoint segments.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Collections
  * 
  * @groupprio  Traversing  1
  * 
  * @define collection  collection
  */
trait Compound[+Segment] extends Any {
  /** Returns a new iterator over the segments of this $collection.
    * @group Traversing */
  def segments: Iterator[Segment]
}
