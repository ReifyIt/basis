/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection
package generic

import language.higherKinds

import scala.collection._
import scala.collection.generic._

/** A factory for sequences with an implicit builder factory.
  * 
  * @author Chris Sachs
  */
abstract class GenericSeqFactory[CC[X] <: GenSeq[X]] {
  def empty[A](implicit bf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = bf().result
  
  def apply[A](values: A*)(implicit bf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = {
    val builder = bf()
    builder ++= values
    builder.result
  }
  
  def unapplySeq[A](seq: CC[A]): Some[CC[A]] = Some(seq)
  
  def concat[A](xss: Traversable[A]*)(implicit bf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = {
    val builder = bf()
    builder.sizeHint(xss.map(_.size).sum) // assuming extra traversal is faster than extra copying
    for (xs <- xss.seq) builder ++= xs
    builder.result
  }
  
  def fill[A](count: Int)(value: => A)(implicit bf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = {
    val builder = bf()
    builder.sizeHint(count)
    var i = 0
    while (i < count) {
      builder += value
      i += 1
    }
    builder.result
  }
  
  def iterate[A](start: A, count: Int)(f: A => A)(implicit bf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = {
    val builder = bf()
    builder.sizeHint(count)
    if (count > 0) {
      var value = start
      builder += value
      var i = 1
      while (i < count) {
        value = f(value)
        builder += value
        i += 1
      }
    }
    builder.result
  }
  
  def tabulate[A](count: Int)(f: Int => A)(implicit bf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = {
    val builder = bf()
    builder.sizeHint(count)
    var i = 0
    while (i < count) {
      builder += f(i)
      i += 1
    }
    builder.result
  }
}
