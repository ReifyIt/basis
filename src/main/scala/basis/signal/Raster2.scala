/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

/** An updateable discrete binary function on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @tparam A   the sample type.
  */
trait Raster2[A] extends Image2[A] { imageA =>
  /** Updates a sample of this image. */
  def update(i: Long, j: Long, sample: A): Unit
  
  /** Replaces a subset of this image with another image. Eagerly updates the
    * samples at the domains' intersection with the values of the other image.
    * The name ''blit'' stands for '''bl'''ock '''i'''mage '''t'''ransfer. */
  def blit[B <: A](that: Image2[B]) {
    val lower1 = math.max(domain.x.lower, that.domain.x.lower)
    val upper1 = math.min(domain.y.upper, that.domain.y.upper)
    val lower2 = math.max(domain.x.lower, that.domain.x.lower)
    val upper2 = math.min(domain.y.upper, that.domain.y.upper)
    var j = lower2
    while (j <= upper2) {
      var i = lower1
      while (i <= upper1) {
        this(i, j) = that(i, j)
        i += 1L
      }
      j += 1L
    }
  }
  
  override def translate(delta: VectorZ2): Raster2[A] = new Translation(delta)
  
  protected class Translation(override val delta: VectorZ2)
    extends super.Translation(delta) with Raster2[A] {
    
    def update(i: Long, j: Long, sample: A): Unit =
      imageA.update(i + delta.x, j + delta.y, sample)
    
    override def translate(delta: VectorZ2): Raster2[A] =
      new imageA.Translation(this.delta + delta)
  }
}
