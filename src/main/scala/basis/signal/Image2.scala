/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

trait Image2[A] { imageA =>
  def min1: Long
  
  def max1: Long
  
  def min2: Long
  
  def max2: Long
  
  def apply(i: Long, j: Long): A
  
  def translate(delta1: Long, delta2: Long): Image2[A] =
    new Translation(delta1, delta2)
  
  def composite[B, C](that: Image2[B])(operator: (A, B) => C): Image2[C] =
    new Composite[B, C](that)(operator)
  
  def ∗ (that: Image2[A])(implicit isRing: A <:< Ring[A]): Image2[A] =
    new DiscreteConvolution[A](that)
  
  def :∗ [B](filter: Image2[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  def ∗: [B](filter: Image2[B])(implicit isVector: A <:< Vector[A, B]): Image2[A] =
    new DiscreteConvolution[B](filter)
  
  protected class Translation(val delta1: Long, val delta2: Long) extends Image2[A] {
    val min1 = imageA.min1 + delta1
    val max1 = imageA.max1 + delta1
    
    val min2 = imageA.min2 + delta2
    val max2 = imageA.max2 + delta2
    
    def apply(i: Long, j: Long): A = imageA(i + delta1, j + delta2)
    
    override def translate(delta1: Long, delta2: Long): Image2[A] =
      new imageA.Translation(this.delta1 + delta1, this.delta2 + delta2)
  }
  
  protected class Composite[B, C](val imageB: Image2[B])(val operator: (A, B) => C) extends Image2[C] {
    val min1 = math.max(imageA.min1, imageB.min1)
    val max1 = math.min(imageA.max1, imageB.max1)
    
    val min2 = math.max(imageA.min2, imageB.min2)
    val max2 = math.min(imageA.max2, imageB.max2)
    
    def apply(i: Long, j: Long): C = operator(imageA(i, j), imageB(i, j))
  }
  
  protected class DiscreteConvolution[B](imageB: Image2[B])(implicit isVector: A <:< Vector[A, B]) extends Image2[A] {
    val min1 = imageA.min1 + imageB.min1
    val max1 = imageA.max1 + imageB.max1
    
    val min2 = imageA.min2 + imageB.min2
    val max2 = imageA.max2 + imageB.max2
    
    def apply(i: Long, j: Long): A = {
      val lower1 = math.max(imageA.min1, i - imageB.max1)
      val upper1 = math.min(imageA.max1, i - imageB.min1)
      val lower2 = math.max(imageA.min2, i - imageB.max2)
      val upper2 = math.min(imageA.max2, i - imageB.min2)
      var y = lower2
      var x = lower1
      var sample = imageA(x, y) :* imageB(i - x, i - y)
      x += 1L
      while (y <= upper2) {
        while (x <= upper1) {
          sample += imageA(x, y) :* imageB(i - x, i - y)
          x += 1L
        }
        y += 1L
        x = lower1
      }
      sample
    }
  }
}
