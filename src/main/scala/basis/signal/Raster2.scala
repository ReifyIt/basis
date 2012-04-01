/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

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
    val lower1 = math.max(this.lower1, that.lower1)
    val upper1 = math.min(this.upper1, that.upper1)
    val lower2 = math.max(this.lower2, that.lower2)
    val upper2 = math.min(this.upper2, that.upper2)
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
  
  override def translate(delta1: Long, delta2: Long): Raster2[A] =
    new Translation(delta1, delta2)
  
  protected class Translation(delta1: Long, delta2: Long)
    extends super.Translation(delta1, delta2) with Raster2[A] {
    
    def update(i: Long, j: Long, sample: A): Unit =
      imageA.update(i + delta1, j + delta2, sample)
    
    override def translate(delta1: Long, delta2: Long): Raster2[A] =
      new imageA.Translation(this.delta1 + delta1, this.delta2 + delta2)
  }
}
