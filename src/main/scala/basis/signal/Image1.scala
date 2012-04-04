/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

/** A discrete unary function on a bounded domain. Supports non-strict,
  * non-destructive algebraic operations.
  * 
  * @author Chris Sachs
  * 
  * @tparam A   the sample type.
  */
trait Image1[A] extends (Long => A) { imageA =>
  /** The bounded domain of this image. */
  def domain: Interval
  
  /** Returns a sample of this image. */
  def apply(i: Long): A
  
  /** Translates the domain of this image. The returned image behaves according
    * to the identity `image(x) = image.translate(dx)(x + dx)`
    * 
    * @param  delta   the amount to offset this image's domain.
    * @return a view of this image with the domain translated.
    */
  def translate(delta: Long): Image1[A] = new Translation(delta)
  
  /** Returns an image that applies a function to each sample of this image.
    * 
    * @tparam B   the sample type of the returned image.
    * @param  f   the function to apply to each sample.
    * @return the non-strict mapping of this image.
    */
  def map[B](f: A => B): Image1[B] = new Map[B](f)
  
  /** Composites this image and another image using an operator function. The
    * returned image's domain is the intersection of this image's domain and
    * the other image's domain.
    * 
    * @tparam B         the sample type of the other image.
    * @tparam C         the sample type of the returned image.
    * @param  that      the other image to composite.
    * @param  operator  the function that combines image samples.
    * @return the composition of corresponding values at each element of the domain.
    */
  def composite[B, C](that: Image1[B])(operator: (A, B) => C): Image1[C] =
    new Composite[B, C](that)(operator)
  
  /** Convolves this image with another image. All samples must lie in the same
    * ring. The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  that    the image to convolve with.
    * @param  isRing  implicit evidence that the images' samples lie in a `Ring`.
    * @return the discrete convolution of this image with the other image.
    */
  def ∗ (that: Image1[A])(implicit isRing: A <:< Ring[A]): Image1[A] =
    new DiscreteConvolution[A](that)
  
  /** Convolves this vector image with a scalar image on the right. The name of
    * this method uses the unicode asterisk operator U+2217.
    * 
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the scalar filter to convolve with.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def :∗ [B](filter: Image1[B])(implicit isVector: A <:< Vector[A, B]): Image1[A] =
    new DiscreteConvolution[B](filter)
  
  /** Convolves this vector image with a scalar image on the left. The name of
    * this method uses the unicode asterisk operator U+2217.
    * 
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the scalar filter to convolve with.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def ∗: [B](filter: Image1[B])(implicit isVector: A <:< Vector[A, B]): Image1[A] =
    new DiscreteConvolution[B](filter)
  
  /** Convolves this vector image with a continuous scalar filter on the right.
    * The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def :∗ [B](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): (Double => A) =
    new ContinuousConvolution[B](filter)
  
  /** Convolves this vector image with a continuous scalar filter on the left.
    * The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def ∗: [B](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): (Double => A) =
    new ContinuousConvolution[B](filter)
  
  protected class Translation(val delta: Long) extends Image1[A] {
    val domain: Interval = imageA.domain + delta
    
    def apply(i: Long): A = imageA(i + delta)
    
    override def translate(delta: Long): Image1[A] =
      new imageA.Translation(this.delta + delta)
  }
  
  protected class Composite[B, C]
      (imageB: Image1[B])(operator: (A, B) => C)
    extends Image1[C] {
    
    val domain: Interval = imageA.domain intersect imageB.domain
    
    def apply(i: Long): C = operator(imageA(i), imageB(i))
  }
  
  protected class Map[B](f: A => B) extends Image1[B] {
    def domain: Interval = imageA.domain
    
    def apply(i: Long): B = f(imageA(i))
  }
  
  protected class DiscreteConvolution[B]
      (imageB: Image1[B])(implicit isVector: A <:< Vector[A, B])
    extends Image1[A] {
    
    val domain: Interval = imageA.domain + imageB.domain
    
    def apply(i: Long): A = {
      val lower = math.max(imageA.domain.lower, i - imageB.domain.upper)
      val upper = math.min(imageA.domain.upper, i - imageB.domain.lower)
      var j = lower
      var sample = imageA(j) :* imageB(i - j)
      j += 1L
      while (j <= upper) {
        sample += imageA(j) :* imageB(i - j)
        j += 1L
      }
      sample
    }
  }
  
  protected class ContinuousConvolution[B]
      (filter: Double => B)(implicit isVector: A <:< Vector[A, B])
    extends (Double => A) {
    
    def apply(x: Double): A = {
      var lower = imageA.domain.lower
      var upper = imageA.domain.upper
      var j = lower
      var sample = imageA(j) :* filter(x - j)
      j += 1L
      while (j <= upper) {
        sample += imageA(j) :* filter(x - j)
        j += 1L
      }
      sample
    } 
  }
}
