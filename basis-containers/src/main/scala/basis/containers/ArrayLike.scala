/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._

import scala.reflect.ClassTag

/** An array-convertible collection.
  * 
  * @groupprio  Examining   -2
  * @groupprio  Converting  -1
  * 
  * @define collection  collection
  */
trait ArrayLike[+A] extends Any { this: Enumerator[A] =>
  /** Returns the number of elements in this $collection.
    * @group Examining */
  def length: Int
  
  /** Copies elements from this $collection to an array slice.
    * 
    * @param  xs      the destination array.
    * @param  start   the offset to copy to in the destination array.
    * @param  count   the maximum number of elements to copy.
    * @group  Converting
    */
  def copyToArray[B >: A](xs: Array[B], start: Int, count: Int): Unit
  
  /** Copies elements from this $collection to an array offset.
    * 
    * @param  xs      the destination array.
    * @param  start   the offset to copy to in the destination array.
    * @group  Converting
    */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit
  
  /** Copies elements from this $collection to an array.
    * 
    * @param  xs  the destination array.
    * @group  Converting
    */
  def copyToArray[B >: A](xs: Array[B]): Unit
  
  /** Returns an array containing the elements of this $collection.
    * 
    * @group  Converting
    * @usecase def toArray: Array[A]
    * @inheritdoc
    */
  def toArray[B >: A](implicit B: ClassTag[B]): Array[B]
}
