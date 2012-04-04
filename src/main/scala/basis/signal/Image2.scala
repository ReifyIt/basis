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
  /** The 2D domain of this image. */
  def domain: IntervalZ2
  
  /** Returns a sample of this image. */
  def apply(i: Long, j: Long): A
  
  /** Translates the domain of this image. The returned image behaves according
    * to this identity: `image(x, y) = image.offset(dx, dy)(x + dx, y + dy)`
    * 
    * @param  delta   the amount to offset this image's domain.
    * @return a view of this image with the domain translated.
    */
  def translate(delta: VectorZ2): Image2[A] = new Translation(delta)
  
  /** Returns an image that applies a function to each sample of this image.
    * 
    * @tparam B   the sample type of the returned image.
    * @param  f   the function to apply to each sample.
    * @return the non-strict mapping of this image.
    */
  def map[B](f: A => B): Image2[B] = new Map[B](f)
  
  /** Composites this image and another image using an operator function. The
    * returned image's domain is the intersection of this image's domain and
    * the other image's domain.
    * 
    * @param  B         the sample type of the other image.
    * @param  C         the sample type of the returned image.
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
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the scalar filter to convolve with.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def :∗ [B](filter: Image2[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  /** Convolves this vector image with a scalar image on the left. The name of
    * this method uses the unicode asterisk operator U+2217.
    * 
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the scalar filter to convolve with.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete convolution of this image with the filter.
    */
  def ∗: [B](filter: Image2[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  /** Convolves this vector image with a 1-dimensional scalar image on the right.
    * Equivalent to a convolving with `(x, y) => filter(x) * filter(y)`.
    * 
    * @param  B         the scalar sample type of the other image.
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
    * @param  B         the scalar sample type of the other image.
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
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def :∗ [B](filter: (Double, Double) => B)(implicit isVector: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousConvolution[B](filter)
  
  /** Convolves this vector image with a continuous scalar filter on the left.
    * The name of this method uses the unicode asterisk operator U+2217.
    * 
    * @param  B         the scalar sample type of the other image.
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
    * @param  B         the scalar sample type of the other image.
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
    * @param  B         the scalar sample type of the other image.
    * @param  filter    the 1-dimensional continuous scalar filter function.
    * @param  isVector  implicit evidence that this image has vector samples.
    * @return the discrete-continuous convolution of this image with the filter.
    */
  def ∗: [B <: Ring[B]](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousSeparableConvolution[B](filter, filter)
  
  protected class Translation(val delta: VectorZ2) extends Image2[A] {
    val domain: IntervalZ2 = imageA.domain + delta
    
    def apply(i: Long, j: Long): A = imageA(i + delta.x, j + delta.y)
    
    override def translate(delta: VectorZ2): Image2[A] =
      new imageA.Translation(this.delta + delta)
  }
  
  protected class Map[B](f: A => B) extends Image2[B] {
    def domain: IntervalZ2 = imageA.domain
    
    def apply(i: Long, j: Long): B = f(imageA(i, j))
  }
  
  protected class Composite[B, C]
      (imageB: Image2[B])(operator: (A, B) => C)
    extends Image2[C] {
    
    val domain: IntervalZ2 = imageA.domain intersect imageB.domain
    
    def apply(i: Long, j: Long): C = operator(imageA(i, j), imageB(i, j))
  }
  
  protected class DiscreteConvolution[B]
      (imageB: Image2[B])(implicit isVector: A <:< Vector[A, B])
    extends Image2[A] {
    
    val domain: IntervalZ2 = imageA.domain + imageB.domain
    
    def apply(i: Long, j: Long): A = {
      val lowerX = math.max(imageA.domain.x.lower, i - imageB.domain.x.upper)
      val upperX = math.min(imageA.domain.x.upper, i - imageB.domain.x.lower)
      val lowerY = math.max(imageA.domain.y.lower, j - imageB.domain.y.upper)
      val upperY = math.min(imageA.domain.y.upper, j - imageB.domain.y.lower)
      var y = lowerY
      var x = lowerX
      var sample = imageA(x, y) :* imageB(i - x, j - y)
      x += 1L
      while (y <= upperY) {
        while (x <= upperX) {
          sample += imageA(x, y) :* imageB(i - x, j - y)
          x += 1L
        }
        y += 1L
        x = lowerX
      }
      sample
    }
  }
  
  protected class DiscreteSeparableConvolution[B <: Ring[B]]
      (imageB1: Image1[B], imageB2: Image1[B])
      (implicit isVector: A <:< Vector[A, B])
    extends Image2[A] {
    
     val domain: IntervalZ2 =
      imageA.domain + new IntervalZ2(imageB1.domain, imageB2.domain)
    
    def apply(i: Long, j: Long): A = {
      // TODO: caching strategy
      val lowerX = math.max(imageA.domain.x.lower, i - imageB1.domain.upper)
      val upperX = math.min(imageA.domain.x.upper, i - imageB1.domain.lower)
      val lowerY = math.max(imageA.domain.y.lower, j - imageB2.domain.upper)
      val upperY = math.min(imageA.domain.y.upper, j - imageB2.domain.lower)
      var y = lowerY
      var x = lowerX
      var sample = imageA(x, y) :* (imageB1(i - x) * imageB2(j - y))
      x += 1L
      while (y <= upperY) {
        while (x <= upperX) {
          sample += imageA(x, y) :* (imageB1(i - x) * imageB2(j - y))
          x += 1L
        }
        y += 1L
        x = lowerX
      }
      sample
    }
  }
  
  protected class ContinuousConvolution[B]
      (filter: (Double, Double) => B)(implicit isVector: A <:< Vector[A, B])
    extends ((Double, Double) => A) {
    
    def apply(i: Double, j: Double): A = {
      var lowerX = imageA.domain.x.lower
      var upperX = imageA.domain.x.upper
      var lowerY = imageA.domain.y.lower
      var upperY = imageA.domain.y.upper
      var y = lowerY
      var x = lowerX
      var sample = imageA(x, y) :* filter(i - x, j - y)
      x += 1L
      while (y <= upperY) {
        while (x <= upperX) {
          sample += imageA(x, y) :* filter(i - x, j - y)
          x += 1L
        }
        y += 1L
        x = lowerX
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
      var lowerX = imageA.domain.x.lower
      var upperX = imageA.domain.x.upper
      var lowerY = imageA.domain.y.lower
      var upperY = imageA.domain.y.upper
      var y = lowerY
      var x = lowerX
      var sample = imageA(x, y) :* (filter1(i - x) * filter2(j - y))
      x += 1L
      while (y <= upperY) {
        while (x <= upperX) {
          sample += imageA(x, y) :* (filter1(i - x) * filter2(j - y))
          x += 1L
        }
        y += 1L
        x = lowerX
      }
      sample
    }
  }
}
