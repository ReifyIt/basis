/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

/** A discrete 2-dimensional image.
  * 
  * @author Chris Sachs
  */
trait Image2[A] extends ((Long, Long) => A) { imageA =>
  /** The lower bound of the 1st dimension of this image's domain. */
  def min1: Long
  
  /** The upper bound of the 1st dimension of this image's domain. */
  def max1: Long
  
  /** The lower bound of the 2nd dimension of this image's domain. */
  def min2: Long
  
  /** The upper bound of the 2nd dimension of this image's domain. */
  def max2: Long
  
  /** Returns a sample of this image.
    * 
    * @param  i   the sample's 1st coordinate; in the interval [`min1`, `max1`].
    * @param  j   the sample's 2nd coordinate; in the interval [`min2`, `max2`].
    * @return the image sample.
    */
  def apply(i: Long, j: Long): A
  
  /** Translates the domain of this image. The returned image behaves according
    * to this identity: `image(x, y) = image.offset(dx, dy)(x + dx, y + dy)`
    * 
    * @param  delta1  the amount to offset the 1st dimension of this image's domain.
    * @param  delta2  the amount to offset the 2nd dimension of this image's domain.
    * @return a view of this image with the domain translated.
    */
  def translate(delta1: Long, delta2: Long): Image2[A] =
    new Translation(delta1, delta2)
  
  /** Composites this image and another image using an operator function. The
    * returned image's domain is the intersection of this image's domain and
    * the other image's domain.
    * 
    * @param  that      the other image to composite.
    * @param  operator  the function that combines image samples.
    * @return an image that composites the samples at each coordinate.
    */
  def composite[B, C](that: Image2[B])(operator: (A, B) => C): Image2[C] =
    new Composite[B, C](that)(operator)
  
  /** Convolves this image with another image. All samples must lie in the same
    * ring. The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  that    the image to convolve with.
    * @param  isRing  implicit evidence that the images' samples lie in a `Ring`.
    * @return the discrete convolution of this image with the other image.
    */
  def ∗ (that: Image2[A])(implicit isRing: A <:< Ring[A]): Image2[A] =
    new DiscreteConvolution[A](that)
  
  /** Convolves this vector image with a scalar image on the right. The name of
    * this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the scalar filter to convolve with.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def :∗ [B](filter: Image2[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  /** Convolves this vector image with a scalar image on the left. The name of
    * this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the scalar filter to convolve with.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def ∗: [B](filter: Image2[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  /** Convolves this vector image with a 1-dimensional scalar image on the right.
    * Equivalent to a convolving with `(x, y) => filter(x) * filter(y)`.
    * 
    * @param  filter    the 1-dimensional scalar filter.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def :∗ [B <: Ring[B]](filter: Image1[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteSeparableConvolution[B](filter, filter)
  
  /** Convolves this vector image with a 1-dimensional scalar image on the left.
    * Equivalent to a convolving with `(x, y) => filter(x) * filter(y)`. The
    * name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the 1-dimensional scalar filter.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def ∗: [B <: Ring[B]](filter: Image1[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteSeparableConvolution[B](filter, filter)
  
  /** Convolves this vector image with a continuous scalar filter on the right.
    * The name of this method uses the unicode asterisk operator U+2217. The
    * name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def :∗ [B](filter: (Double, Double) => B)(implicit isVector: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousConvolution[B](filter)
  
  /** Convolves this vector image with a continuous scalar filter on the left.
    * The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def ∗: [B](filter: (Double, Double) => B)(implicit isVector: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousConvolution[B](filter)
  
  /** Convolves this vector image with a 1-dimensional continuous scala filter on
    * the right. Equivalent to convolving with `(x, y) => filter(x) * filter(y)`.
    * The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the 1-dimensional continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def :∗ [B <: Ring[B]](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousSeparableConvolution[B](filter, filter)
  
  /** Convolves this vector image with a 1-dimensional continuous scala filter on
    * the left. Equivalent to convolving with `(x, y) => filter(x) * filter(y)`.
    * The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  filter    the 1-dimensional continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def ∗: [B <: Ring[B]](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousSeparableConvolution[B](filter, filter)
  
  protected class Translation(val delta1: Long, val delta2: Long) extends Image2[A] {
    val min1 = imageA.min1 + delta1
    val max1 = imageA.max1 + delta1
    val min2 = imageA.min2 + delta2
    val max2 = imageA.max2 + delta2
    
    def apply(i: Long, j: Long): A = imageA(i + delta1, j + delta2)
    
    override def translate(delta1: Long, delta2: Long): Image2[A] =
      new imageA.Translation(this.delta1 + delta1, this.delta2 + delta2)
  }
  
  protected class Composite[B, C]
      (imageB: Image2[B])(operator: (A, B) => C)
    extends Image2[C] {
    
    val min1 = math.max(imageA.min1, imageB.min1)
    val max1 = math.min(imageA.max1, imageB.max1)
    val min2 = math.max(imageA.min2, imageB.min2)
    val max2 = math.min(imageA.max2, imageB.max2)
    
    def apply(i: Long, j: Long): C = operator(imageA(i, j), imageB(i, j))
  }
  
  protected class DiscreteConvolution[B]
      (imageB: Image2[B])(implicit isVector: A <:< Vector[A, B])
    extends Image2[A] {
    
    val min1 = imageA.min1 + imageB.min1
    val max1 = imageA.max1 + imageB.max1
    val min2 = imageA.min2 + imageB.min2
    val max2 = imageA.max2 + imageB.max2
    
    def apply(i: Long, j: Long): A = {
      val lower1 = math.max(imageA.min1, i - imageB.max1)
      val upper1 = math.min(imageA.max1, i - imageB.min1)
      val lower2 = math.max(imageA.min2, j - imageB.max2)
      val upper2 = math.min(imageA.max2, j - imageB.min2)
      var y = lower2
      var x = lower1
      var sample = imageA(x, y) :* imageB(i - x, j - y)
      x += 1L
      while (y <= upper2) {
        while (x <= upper1) {
          sample += imageA(x, y) :* imageB(i - x, j - y)
          x += 1L
        }
        y += 1L
        x = lower1
      }
      sample
    }
  }
  
  protected class DiscreteSeparableConvolution[B <: Ring[B]]
      (imageB1: Image1[B], imageB2: Image1[B])
      (implicit isVector: A <:< Vector[A, B])
    extends Image2[A] {
    
    val min1 = imageA.min1 + imageB1.min
    val max1 = imageA.max1 + imageB1.max
    val min2 = imageA.min2 + imageB2.min
    val max2 = imageA.max2 + imageB2.max
    
    def apply(i: Long, j: Long): A = {
      // TODO: caching strategy
      val lower1 = math.max(imageA.min1, i - imageB1.max)
      val upper1 = math.min(imageA.max1, i - imageB1.min)
      val lower2 = math.max(imageA.min2, j - imageB2.max)
      val upper2 = math.min(imageA.max2, j - imageB2.min)
      var y = lower2
      var x = lower1
      var sample = imageA(x, y) :* (imageB1(i - x) * imageB2(j - y))
      x += 1L
      while (y <= upper2) {
        while (x <= upper1) {
          sample += imageA(x, y) :* (imageB1(i - x) * imageB2(j - y))
          x += 1L
        }
        y += 1L
        x = lower1
      }
      sample
    }
  }
  
  protected class ContinuousConvolution[B]
      (filter: (Double, Double) => B)(implicit isVector: A <:< Vector[A, B])
    extends ((Double, Double) => A) {
    
    def apply(i: Double, j: Double): A = {
      var lower1 = imageA.min1
      var upper1 = imageA.max1
      var lower2 = imageA.min2
      var upper2 = imageA.max2
      var y = lower2
      var x = lower1
      var sample = imageA(x, y) :* filter(i - x, j - y)
      x += 1L
      while (y <= upper2) {
        while (x <= upper1) {
          sample += imageA(x, y) :* filter(i - x, j - y)
          x += 1L
        }
        y += 1L
        x = lower1
      }
      sample
    }
  }
  
  protected class ContinuousSeparableConvolution[B <: Ring[B]]
      (filter1: Double => B, filter2: Double => B)
      (implicit isVector: A <:< Vector[A, B])
    extends ((Double, Double) => A) {
    
    def apply(i: Double, j: Double): A = {
      // TODO: ad-hoc caching strategy
      var lower1 = imageA.min1
      var upper1 = imageA.max1
      var lower2 = imageA.min2
      var upper2 = imageA.max2
      var y = lower2
      var x = lower1
      var sample = imageA(x, y) :* (filter1(i - x) * filter2(j - y))
      x += 1L
      while (y <= upper2) {
        while (x <= upper1) {
          sample += imageA(x, y) :* (filter1(i - x) * filter2(j - y))
          x += 1L
        }
        y += 1L
        x = lower1
      }
      sample
    }
  }
}
