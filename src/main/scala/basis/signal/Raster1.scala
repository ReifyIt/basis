/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

/** An updateable discrete unary function on a bounded domain.
  * 
  * @author Chris Sachs
  * 
  * @tparam A   the sample type.
  */
trait Raster1[A] extends Image1[A] { imageA =>
  /** Updates a sample of this image. */
  def update(i: Long, sample: A): Unit
  
  /** Replaces a subset of this image with another image. Eagerly updates the
    * samples at the domains' intersection with the values of the other image.
    * The name ''blit'' stands for '''bl'''ock '''i'''mage '''t'''ransfer. */
  def blit[B <: A](that: Image1[B]) {
    val lower = math.max(domain.lower, that.domain.lower)
    val upper = math.min(domain.upper, that.domain.upper)
    var i = lower
    while (i <= upper) {
      this(i) = that(i)
      i += 1L
    }
  }
  
  override def translate(delta: Long): Raster1[A] = new Translation(delta)
  
  protected class Translation(override val delta: Long)
    extends super.Translation(delta) with Raster1[A] {
    
    def update(i: Long, sample: A): Unit =
      imageA.update(i + delta, sample)
    
    override def translate(delta: Long): Raster1[A] =
      new imageA.Translation(this.delta + delta)
  }
}
