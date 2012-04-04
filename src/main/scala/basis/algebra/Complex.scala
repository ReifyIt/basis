/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A complex number modeled by a `Double` real part and a `Double` imaginary part.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a `Complex` value with a real part and an imaginary part.
  * @param  real        The real part.
  * @param  imaginary   The imaginary part.
  * 
  * @define Element   Complex
  * @define element   `Complex` value
  * @define scalar    `Complex` value
  */
final class Complex(val real: Double, val imaginary: Double) extends CompleteField[Complex] {
  /** Returns `true` if this $element is not a number. */
  def isNaN: Boolean = java.lang.Double.isNaN(real) || java.lang.Double.isNaN(imaginary)
  
  /** Returns `true` if this $element is infinite. */
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
  
  def reciprocal: Complex = {
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
  
  /** Returns the exponential of this $element. */
  def exp: Complex =
    new Complex(
      math.exp(real) * math.cos(imaginary),
      math.exp(real) * math.sin(imaginary))
  
  /** Returns the natural logarithm of this $element. */
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
  
  /** Returns the conjugate of this $element. */
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
    if (isNaN) "NaN"
    else if (isInfinite) "Infinity"
    else if (java.lang.Double.compare(imaginary, 0.0) < 0)
      "("+ real +" - "+ -imaginary +"i"+")"
    else
      "("+ real +" + "+  imaginary +"i"+")"
  }
}

/** Contains factory methods and implicit conversions for `Complex` values. */
object Complex {
  /** The zero `Complex` value. */
  val zero: Complex = new Complex(0.0, 0.0)
  
  /** The unit `Complex` value. */
  val one: Complex = new Complex(1.0, 0.0)
  
  /** The imaginary unit `Complex` value. */
  val i: Complex = new Complex(0.0, 1.0)
  
  def apply(real: Double, imaginary: Double): Complex = new Complex(real, imaginary)
  
  def unapply(complex: Complex): Some[(Double, Double)] = Some(complex.real, complex.imaginary)
  
  /** Constructs a `Complex` value with polar coordinates.
    * 
    * @param  r   the modulus of the complex number.
    * @param  φ   the argument of the complex number.
    * @return a new `Complex` value.
    */
  def polar(r: Double, φ: Double): Complex = new Complex(r * math.cos(φ), r * math.sin(φ))
  
  /** Implicitly converts a `Double` value to a real `Complex` value. */
  implicit def real(value: Double): Complex = new Complex(value, 0.0)
  
  /** Converts a `Double` value to an imaginary `Complex` value. */
  def imaginary(value: Double): Complex = new Complex(0.0, value)
  
  /** The additive identity typeclass for the `Complex` field. */
  implicit val additiveIdentity = new Zero(zero)
  
  /** The multiplicative identity typeclass for the `Complex` field. */
  implicit val multiplicativeIdentity = new One(one)
  
  /** The euclidean norm typeclass for the `Complex` field. */
  implicit val euclideanNorm = Norm[Complex, Real] {
    complex => new Real(math.hypot(complex.real, complex.imaginary))
  }
  
  /** The default struct for `Complex` values. */
  implicit lazy val struct = new StructComplex
  
  /** A struct for `Complex` values. */
  class StructComplex(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Double, Double, Complex](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `real` field projection of this struct. */
    def real: Struct[Double] = field1
    
    /** The `imaginary` field projection of this struct. */
    def imaginary: Struct[Double] = field2
    
    def load(data: Data, address: Long): Complex = {
      val real      = data.loadDouble(address + offset1)
      val imaginary = data.loadDouble(address + offset2)
      new Complex(real, imaginary)
    }
    
    def store(data: Data, address: Long, complex: Complex) {
      data.storeDouble(address + offset1, complex.real)
      data.storeDouble(address + offset2, complex.imaginary)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructComplex =
      new StructComplex(offset1 + offset, size, alignment)
    
    override def toString: String = "StructComplex"
  }
}
