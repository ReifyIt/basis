/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

trait MutableImage2[A] extends Image2[A] { imageA =>
  def update(i: Long, j: Long, sample: A): Unit
  
  def blit[B <: A](that: Image2[B]) {
    val lower1 = math.max(this.min1, that.min1)
    val upper1 = math.min(this.max1, that.max1)
    val lower2 = math.max(this.min2, that.min2)
    val upper2 = math.min(this.max2, that.max2)
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
  
  override def translate(delta1: Long, delta2: Long): MutableImage2[A] =
    new Translation(delta1, delta2)
  
  protected class Translation(delta1: Long, delta2: Long)
    extends super.Translation(delta1, delta2) with MutableImage2[A] {
    
    def update(i: Long, j: Long, sample: A): Unit =
      imageA.update(i + delta1, j + delta2, sample)
    
    override def translate(delta1: Long, delta2: Long): MutableImage2[A] =
      new imageA.Translation(this.delta1 + delta1, this.delta2 + delta2)
  }
}
