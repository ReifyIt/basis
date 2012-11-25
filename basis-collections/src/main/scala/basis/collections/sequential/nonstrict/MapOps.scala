/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package nonstrict

import traversable._

/** Non-strictly evaluated map operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  map
  */
trait MapOps[+A, +T, +From]
  extends Any
    with ContainerOps[(A, T), From]
    with general.MapOps[A, T, From] {
  
  protected[this] override def self: Map[A, T]
  
  override def filter(p: ((A, T)) => Boolean): Map[A, T] =
    new MapView.Filter(self, p)
  
  override def dropWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new MapView.DropWhile(self, p)
  
  override def takeWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new MapView.TakeWhile(self, p)
  
  override def span(p: ((A, T)) => Boolean): (Map[A, T], Map[A, T]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Map[A, T] =
    new MapView.Drop(self, lower)
  
  override def take(upper: Int): Map[A, T] =
    new MapView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Map[A, T] =
    new MapView.Slice(self, lower, upper)
}

class MapView[+A, +T, +From](override val self: Map[A, T])
  extends AnyVal with MapOps[A, T, From] {
  
  override def filter(p: ((A, T)) => Boolean): Map[A, T] =
    new MapView.Filter(self, p)
  
  override def dropWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new MapView.DropWhile(self, p)
  
  override def takeWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new MapView.TakeWhile(self, p)
  
  override def span(p: ((A, T)) => Boolean): (Map[A, T], Map[A, T]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Map[A, T] =
    new MapView.Drop(self, lower)
  
  override def take(upper: Int): Map[A, T] =
    new MapView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Map[A, T] =
    new MapView.Slice(self, lower, upper)
}

private[nonstrict] object MapView {
  import scala.annotation.unchecked.uncheckedVariance
  
  class Filter[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends ContainerView.Filter[(A, T)](base, p) with Map[A, T]
  
  class DropWhile[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends ContainerView.DropWhile[(A, T)](base, p) with Map[A, T]
  
  class TakeWhile[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends ContainerView.TakeWhile[(A, T)](base, p) with Map[A, T]
  
  class Drop[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val lower: Int)
    extends ContainerView.Drop[(A, T)](base, lower) with Map[A, T]
  
  class Take[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val upper: Int)
    extends ContainerView.Take[(A, T)](base, upper) with Map[A, T]
  
  class Slice[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends ContainerView.Slice[(A, T)](base, lower, upper) with Map[A, T]
}
