/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.signal

import basis.algebra._

trait Image1[A] { imageA =>
  def apply(i: Long): A
  
  def min: Long
  
  def max: Long
  
  def translate(delta: Long): Image1[A] = new Translation(delta)
  
  def composite[B, C](that: Image1[B])(operator: (A, B) => C): Image1[C] =
    new Composite[B, C](that)(operator)
  
  def ∗ (that: Image1[A])(implicit isRing: A <:< Ring[A]): Image1[A] =
    new DiscreteConvolution[A](that)
  
  def :∗ [B](filter: Image1[B])(implicit isVector: A <:< Vector[A, B]): Image1[A] =
    new DiscreteConvolution[B](filter)
  
  def ∗: [B](filter: Image1[B])(implicit isVector: A <:< Vector[A, B]): Image1[A] =
    new DiscreteConvolution[B](filter)
  
  def :∗ [B](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): (Double => A) =
    new ContinuousConvolution[B](filter)
  
  def ∗: [B](filter: Double => B)(implicit isVector: A <:< Vector[A, B]): (Double => A) =
    new ContinuousConvolution[B](filter)
  
  protected class Translation(val delta: Long) extends Image1[A] {
    val min = imageA.min + delta
    val max = imageA.max + delta
    
    def apply(i: Long): A = imageA(i + delta)
    
    override def translate(delta: Long): Image1[A] =
      new imageA.Translation(this.delta + delta)
  }
  
  protected class Composite[B, C]
      (imageB: Image1[B])(operator: (A, B) => C)
    extends Image1[C] {
    
    val min = math.max(imageA.min, imageB.min)
    val max = math.min(imageA.max, imageB.max)
    
    def apply(i: Long): C = operator(imageA(i), imageB(i))
  }
  
  protected class DiscreteConvolution[B]
      (imageB: Image1[B])(implicit isVector: A <:< Vector[A, B])
    extends Image1[A] {
    
    val min = imageA.min + imageB.min
    val max = imageA.max + imageB.max
    
    def apply(i: Long): A = {
      val lower = math.max(imageA.min, i - imageB.max)
      val upper = math.min(imageA.max, i - imageB.min)
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
      var lower = imageA.min
      var upper = imageA.max
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
