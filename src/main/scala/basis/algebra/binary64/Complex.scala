/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import basis.util.MurmurHash._

final class Complex(val real: Double, val imaginary: Double) extends CompleteField {
  type Space  = Complex.type
  type Scalar = Complex
  
  def Space = Complex
  
  def isNaN: Boolean = java.lang.Double.isNaN(real) || java.lang.Double.isNaN(imaginary)
  
  def isInfinite: Boolean = !isNaN && (java.lang.Double.isInfinite(real) || java.lang.Double.isInfinite(imaginary))
  
  def + (that: Complex): Complex = new Complex(real + that.real, imaginary + that.imaginary)
  
  def + (x: Double): Complex = new Complex(real + x, imaginary)
  
  def unary_- : Complex = new Complex(-real, -imaginary)
  
  def - (that: Complex): Complex = new Complex(real - that.real, imaginary - that.imaginary)
  
  def - (x: Double): Complex = new Complex(real - x, imaginary)
  
  def * (that: Complex): Complex =
    new Complex(
      real * that.real - imaginary * that.imaginary,
      imaginary * that.real + real * that.imaginary)
  
  def * (x: Double): Complex = new Complex(real * x, imaginary * x)
  
  def inverse: Complex = {
    val absoluteSquare = real * real + imaginary * imaginary
    new Complex(real / absoluteSquare, -imaginary / absoluteSquare)
  }
  
  def / (that: Complex): Complex = {
    if (math.abs(that.real) >= math.abs(that.imaginary)) {
      val scale = that.imaginary / that.real
      val unscale = that.real + that.imaginary * scale
      new Complex(
        (real + imaginary * scale) / unscale,
        (imaginary - real * scale) / unscale)
    }
    else {
      val scale = that.real / that.imaginary
      val unscale = that.imaginary + that.real * scale
      new Complex(
        (imaginary + real * scale) / unscale,
        (real - imaginary * scale) / unscale)
    }
  }
  
  def / (x: Double): Complex = new Complex(real / x, imaginary / x)
  
  def pow(that: Complex): Complex = (this.log * that).exp
  
  def pow(x: Double): Complex = (this.log * x).exp
  
  def exp: Complex =
    new Complex(
      math.exp(real) * math.cos(imaginary),
      math.exp(real) * math.sin(imaginary))
  
  def log: Complex = {
    val modulus = math.hypot(real, imaginary)
    val argument = math.atan2(imaginary, real)
    new Complex(math.log(modulus), argument)
  }
  
  def sqrt: Complex = {
    val r = math.sqrt(math.hypot(real, imaginary))
    val φ = math.atan2(imaginary, real) / 2.0
    new Complex(r * math.cos(φ), r * math.sin(φ))
  }
  
  def conjugate: Complex = new Complex(real, -imaginary)
  
  override def equals(other: Any): Boolean = other match {
    case that: Complex =>
      if (this.isNaN || that.isNaN) false
      else real == that.real && imaginary == that.imaginary
    case _ => false
  }
  
  override def hashCode: Int = {
    if (isNaN) mash(mix(mix(-255095270, Double.NaN), Double.NaN))
    else mash(mix(mix(-255095270, real), imaginary))
  }
  
  override def toString: String = {
    if (java.lang.Double.compare(imaginary, 0.0) < 0)
      "("+ real +" - "+ -imaginary +"i"+")"
    else
      "("+ real +" + "+  imaginary +"i"+")"
  }
}

object Complex extends ScalarSpace {
  type Scalar = Complex
  
  val zero: Complex = new Complex(0.0, 0.0)
  
  val unit: Complex = new Complex(1.0, 0.0)
  
  val i: Complex = new Complex(0.0, 1.0)
  
  def apply(real: Double, imaginary: Double): Complex = new Complex(real, imaginary)
  
  def apply(value: Double): Complex = new Complex(value, 0.0)
  
  def apply(value: Float): Complex = new Complex(value, 0.0)
  
  def apply(value: Long): Complex = new Complex(value, 0.0)
  
  def apply(value: Int): Complex = new Complex(value, 0.0)
  
  def unapply(complex: Complex): Some[(Double, Double)] = Some(complex.real, complex.imaginary)
  
  implicit def real(value: Double): Complex = new Complex(value, 0.0)
  
  def imaginary(value: Double): Complex = new Complex(0.0, value)
  
  def polar(r: Double, φ: Double): Complex = new Complex(r * math.cos(φ), r * math.sin(φ))
  
  override def toString: String = "Complex"
}
