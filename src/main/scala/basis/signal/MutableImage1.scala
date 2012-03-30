/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

trait MutableImage1[A] extends Image1[A] { imageA =>
  def update(i: Long, sample: A): Unit
  
  def blit[B <: A](that: Image1[B]) {
    val lower = math.max(this.min, that.min)
    val upper = math.min(this.max, that.max)
    var i = lower
    while (i <= upper) {
      this(i) = that(i)
      i += 1L
    }
  }
  
  override def translate(delta: Long): MutableImage1[A] = new Translation(delta)
  
  protected class Translation(delta: Long) extends super.Translation(delta) with MutableImage1[A] {
    def update(i: Long, sample: A): Unit =
      imageA.update(i + delta, sample)
    
    override def translate(delta: Long): MutableImage1[A] =
      new imageA.Translation(this.delta + delta)
  }
}
