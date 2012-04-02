/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

/** A discrete binary function on a bounded domain. Supports non-strict,
  * non-destructive algebraic operations.
  * 
  * @author Chris Sachs
  * 
  * @tparam A   the sample type.
  */
trait Image2[A] extends ((Long, Long) => A) { imageA =>
  /** The lower bound of the domain's first component. */
  def lower1: Long
  
  /** The upper bound of the domain's first component. */
  def upper1: Long
  
  /** The lower bound of the domain's second component. */
  def lower2: Long
  
  /** The upper bound of the domain's second component. */
  def upper2: Long
  
  /** Returns a sample of this image. */
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
    * @return the composition of corresponding values at each element of the domain.
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
    val lower1 = imageA.lower1 + delta1
    val upper1 = imageA.upper1 + delta1
    val lower2 = imageA.lower2 + delta2
    val upper2 = imageA.upper2 + delta2
    
    def apply(i: Long, j: Long): A = imageA(i + delta1, j + delta2)
    
    override def translate(delta1: Long, delta2: Long): Image2[A] =
      new imageA.Translation(this.delta1 + delta1, this.delta2 + delta2)
  }
  
  protected class Composite[B, C]
      (imageB: Image2[B])(operator: (A, B) => C)
    extends Image2[C] {
    
    val lower1 = math.max(imageA.lower1, imageB.lower1)
    val upper1 = math.min(imageA.upper1, imageB.upper1)
    val lower2 = math.max(imageA.lower2, imageB.lower2)
    val upper2 = math.min(imageA.upper2, imageB.upper2)
    
    def apply(i: Long, j: Long): C = operator(imageA(i, j), imageB(i, j))
  }
  
  protected class DiscreteConvolution[B]
      (imageB: Image2[B])(implicit isVector: A <:< Vector[A, B])
    extends Image2[A] {
    
    val lower1 = imageA.lower1 + imageB.lower1
    val upper1 = imageA.upper1 + imageB.upper1
    val lower2 = imageA.lower2 + imageB.lower2
    val upper2 = imageA.upper2 + imageB.upper2
    
    def apply(i: Long, j: Long): A = {
      val lower1 = math.max(imageA.lower1, i - imageB.upper1)
      val upper1 = math.min(imageA.upper1, i - imageB.lower1)
      val lower2 = math.max(imageA.lower2, j - imageB.upper2)
      val upper2 = math.min(imageA.upper2, j - imageB.lower2)
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
    
    val lower1 = imageA.lower1 + imageB1.lower
    val upper1 = imageA.upper1 + imageB1.upper
    val lower2 = imageA.lower2 + imageB2.lower
    val upper2 = imageA.upper2 + imageB2.upper
    
    def apply(i: Long, j: Long): A = {
      // TODO: caching strategy
      val lower1 = math.max(imageA.lower1, i - imageB1.upper)
      val upper1 = math.min(imageA.upper1, i - imageB1.lower)
      val lower2 = math.max(imageA.lower2, j - imageB2.upper)
      val upper2 = math.min(imageA.upper2, j - imageB2.lower)
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
      var lower1 = imageA.lower1
      var upper1 = imageA.upper1
      var lower2 = imageA.lower2
      var upper2 = imageA.upper2
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
      var lower1 = imageA.lower1
      var upper1 = imageA.upper1
      var lower2 = imageA.lower2
      var upper2 = imageA.upper2
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
