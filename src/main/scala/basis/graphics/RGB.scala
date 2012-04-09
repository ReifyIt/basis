/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.graphics

import basis.algebra._
import basis.util.MurmurHash._

object RGB extends ColorModel {
  type Scalar = Real
  
  final class Color(val a: Double)(val r: Double, val g: Double, val b: Double)
    extends RealVector[Color] {
    
    def this(r: Double, g: Double, b: Double) = this(1.0)(r, g, b)
    
    def + (that: Color): Color =
      new Color(a + that.a)(r + that.r, g + that.g, b + that.b)
    
    def unary_- : Color = new Color(-a)(-r, -g, -b)
    
    def - (that: Color): Color =
      new Color(a - that.a)(r - that.r, g - that.g, b - that.b)
    
    def :* (scalar: Double): Color =
      new Color(a * scalar)(r * scalar, g * scalar, b * scalar)
    
    def *: (scalar: Double): Color = this :* scalar
    
    def / (scalar: Double): Color =
      new Color(a / scalar)(r / scalar, g / scalar, b / scalar)
    
    def over(that: Color): Color = {
      val v = 1.0 - a
      new Color(a + v * that.a)(r + v * that.r, g + v * that.g, b + v * that.b)
    }
    
    def in(that: Color): Color = {
      val u = that.a
      new Color(u * a)(u * r, u * g, u * b)
    }
    
    def out(that: Color): Color = {
      val u = (1.0 - that.a)
      new Color(u * a)(u * r, u * g, u * b)
    }
    
    def atop(that: Color): Color = {
      val u = that.a
      val v = 1.0 - a
      new Color(u * a + v * that.a)(u * r + v * that.r, u * g + v * that.g, u * b + v * that.b)
    }
    
    def xor(that: Color): Color = {
      val u = 1.0 - that.a
      val v = 1.0 - a
      new Color(u * a + v * that.a)(u * r + v * that.r, u * g + v * that.g, u * b + v * that.b)
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Color => a == that.a && r == that.r && g == that.g && b == that.b
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(mix(mix(-1783290161, a), r), g), b))
    
    override def toString: String = {
      if (a == 1.0) "RGB"+"("+ r +", "+ g +", "+ b +")"
      else "RGB"+"("+ a +")"+"("+ r +", "+ g +", "+ b +")"
    }
  }
  
  val Scalar = Real
  
  val zero: Color = new Color(0.0)(0.0, 0.0, 0.0)
  
  def apply(a: Double)(r: Double, g: Double, b: Double): Color =
    new Color(a)(r, g, b)
  
  def apply(r: Double, g: Double, b: Double): Color =
    new Color(r, g, b)
  
  def unapply(color: Color): Some[(Double, Double, Double)] =
    Some(color.r, color.g, color.b)
  
  override def toString: String = "RGB"
}
