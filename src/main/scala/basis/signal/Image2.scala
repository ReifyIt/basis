/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

trait Image2[A] { imageA =>
  def apply(i: Long, j: Long): A
  
  def min1: Long
  
  def max1: Long
  
  def min2: Long
  
  def max2: Long
  
  def translate(delta1: Long, delta2: Long): Image2[A] =
    new Translation(delta1, delta2)
  
  def composite[B, C](that: Image2[B])(operator: (A, B) => C): Image2[C] =
    new Composite[B, C](that)(operator)
  
  def ∗ (that: Image2[A])(implicit isRingA: A <:< Ring[A]): Image2[A] =
    new DiscreteConvolution[A](that)
  
  def :∗ [B](filter: Image2[B])(implicit isVectorAB: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  def ∗: [B](filter: Image2[B])(implicit isVectorAB: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  def :∗ [B](filter: Image1[B])(implicit isVectorAB: A <:< Vector[A, B], isRingB: B <:< Ring[B]): Image2[A] =
    new DiscreteSeparableConvolution[B](filter, filter)
  
  def ∗: [B](filter: Image1[B])(implicit isVectorAB: A <:< Vector[A, B], isRingB: B <:< Ring[B]): Image2[A] =
    new DiscreteSeparableConvolution[B](filter, filter)
  
  def :∗ [B](filter: (Double, Double) => B)(implicit isVectorAB: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousConvolution[B](filter)
  
  def ∗: [B](filter: (Double, Double) => B)(implicit isVectorAB: A <:< Vector[A, B]): ((Double, Double) => A) =
    new ContinuousConvolution[B](filter)
  
  def :∗ [B](filter: Double => B)(implicit isVectorAB: A <:< Vector[A, B], isRingB: B <:< Ring[B]): ((Double, Double) => A) =
    new ContinuousSeparableConvolution[B](filter, filter)
  
  def ∗: [B](filter: Double => B)(implicit isVectorAB: A <:< Vector[A, B], isRingB: B <:< Ring[B]): ((Double, Double) => A) =
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
      (imageB: Image2[B])(implicit isVectorAB: A <:< Vector[A, B])
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
  
  protected class DiscreteSeparableConvolution[B]
      (imageB1: Image1[B], imageB2: Image1[B])
      (implicit isVectorAB: A <:< Vector[A, B], isRingB: B <:< Ring[B])
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
      (filter: (Double, Double) => B)(implicit isVectorAB: A <:< Vector[A, B])
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
  
  protected class ContinuousSeparableConvolution[B]
      (filter1: Double => B, filter2: Double => B)
      (implicit isVectorAB: A <:< Vector[A, B], isRingB: B <:< Ring[B])
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
