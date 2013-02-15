/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.implicitNotFound

/** An array-compatible collection builder.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Builders
  * 
  * @groupprio  Inserting   1
  * @groupprio  Removing    2
  * @groupprio  Exporting   3
  * 
  * @define collection  array builder
  */
@implicitNotFound("No array builder available for element type ${A}.")
trait ArrayBuilder[A] extends Builder[A] {
  /** Appends an array of elements to this $collection.
    * @group Inserting */
  def appendArray(elems: Array[A]) {
    var i = 0
    val n = elems.length
    while (i < n) {
      append(elems(i))
      i += 1
    }
  }
  
  /** Appends an array of elements to this $collection.
    * @group Inserting */
  def ++= (elems: Array[A]): this.type = {
    appendArray(elems)
    this
  }
  
  override def expect(count: Int): this.type
}
