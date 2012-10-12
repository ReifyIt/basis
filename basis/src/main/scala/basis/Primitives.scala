/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

trait Primitives { this: Aliases =>
  import scala.language.experimental.macros
  import scala.language.implicitConversions
  
  
  class ByteType extends Hash[Byte] with Show[Byte] {
    final def MinValue: Byte = macro ByteMacros.MinValue
    
    final def MaxValue: Byte = macro ByteMacros.MaxValue
    
    override def equal(x: Byte, y: Byte): Boolean = x == y
    
    override def hash(x: Byte): Int = x.##
    
    override def show(x: Byte)(implicit buffer: CharBuffer): buffer.State = Int.show(x)
  }
  
  implicit val Byte: ByteType
  
  
  class ShortType extends Hash[Short] with Show[Short] {
    final def MinValue: Short = macro ShortMacros.MinValue
    
    final def MaxValue: Short = macro ShortMacros.MaxValue
    
    override def equal(x: Short, y: Short): Boolean = x == y
    
    override def hash(x: Short): Int = x.##
    
    override def show(x: Short)(implicit buffer: CharBuffer): buffer.State = Int.show(x)
  }
  
  implicit val Short: ShortType
  
  
  abstract class IntOps {
    def abs: Int = macro IntMacros.abs
    
    def min(that: Int): Int = macro IntMacros.min
    
    def max(that: Int): Int = macro IntMacros.max
    
    def signum: Int = macro IntMacros.signum
    
    def bitCount: Int = macro IntMacros.bitCount
    
    def countLeadingZeros: Int = macro IntMacros.countLeadingZeros
    
    def countTrailingZeros: Int = macro IntMacros.countTrailingZeros
    
    def toFloatBits: Float = macro IntMacros.toFloatBits
  }
  
  class IntType extends Hash[Int] with Show[Int] {
    final def MinValue: Int = macro IntMacros.MinValue
    
    final def MaxValue: Int = macro IntMacros.MaxValue
    
    override def equal(x: Int, y: Int): Boolean = x == y
    
    override def hash(x: Int): Int = x.##
    
    override def show(x: Int)(implicit buffer: CharBuffer): buffer.State = {
      if (x < 0) buffer += new Char('-')
      var ds = new scala.Array[scala.Char](10)
      var d = x
      // factor signed high digit in case of 0 or MinValue
      ds(0) = ('0' + java.lang.Math.abs(d % 10)).toChar
      d = java.lang.Math.abs(d / 10)
      var i = 0
      while (d != 0) {
        i += 1
        ds(i) = ('0' + (d % 10)).toChar
        d /= 10
      }
      while (i >= 0) {
        buffer += new Char(ds(i))
        i -= 1
      }
      buffer.check
    }
  }
  
  implicit def IntOps(value: Int): IntOps =
    throw new java.lang.UnsupportedOperationException
  
  implicit val Int: IntType
  
  
  abstract class LongOps {
    def abs: Long = macro LongMacros.abs
    
    def min(that: Long): Long = macro LongMacros.min
    
    def max(that: Long): Long = macro LongMacros.max
    
    def signum: Int = macro LongMacros.signum
    
    def bitCount: Int = macro LongMacros.bitCount
    
    def countLeadingZeros: Int = macro LongMacros.countLeadingZeros
    
    def countTrailingZeros: Int = macro LongMacros.countTrailingZeros
    
    def toDoubleBits: Double = macro LongMacros.toDoubleBits
  }
  
  class LongType extends Hash[Long] with Show[Long] {
    final def MinValue: Long = macro LongMacros.MinValue
    
    final def MaxValue: Long = macro LongMacros.MaxValue
    
    override def equal(x: Long, y: Long): Boolean = x == y
    
    override def hash(x: Long): Int = x.##
    
    override def show(x: Long)(implicit buffer: CharBuffer): buffer.State = {
      if (x < 0L) buffer += new Char('-')
      var ds = new scala.Array[scala.Char](19)
      var d = x
      // factor signed high digit in case of 0 or MinValue
      ds(0) = ('0' + java.lang.Math.abs(d % 10L).toInt).toChar
      d = java.lang.Math.abs(d / 10L)
      var i = 0
      while (d != 0) {
        i += 1
        ds(i) = ('0' + (d % 10L).toInt).toChar
        d /= 10L
      }
      while (i >= 0) {
        buffer += new Char(ds(i))
        i -= 1
      }
      buffer += new Char('L')
      buffer.check
    }
  }
  
  implicit def LongOps(value: Long): LongOps =
    throw new java.lang.UnsupportedOperationException
  
  implicit val Long: LongType
  
  
  abstract class FloatOps {
    def abs: Float = macro FloatMacros.abs
    
    def min(that: Float): Float = macro FloatMacros.min
    
    def max(that: Float): Float = macro FloatMacros.max
    
    def toIntBits: Int = macro FloatMacros.toIntBits
  }
  
  class FloatType extends Hash[Float] with Show[Float] {
    final def MinValue: Float = macro FloatMacros.MinValue
    
    final def MaxValue: Float = macro FloatMacros.MaxValue
    
    final def Infinity: Float = macro FloatMacros.Infinity
    
    final def NaN: Float = macro FloatMacros.NaN
    
    override def equal(x: Float, y: Float): Boolean = x == y
    
    override def hash(x: Float): Int = x.##
    
    override def show(x: Float)(implicit buffer: CharBuffer): buffer.State = {
      buffer.append(java.lang.Float.toString(x))
      buffer.check
    }
  }
  
  implicit def FloatOps(value: Float): FloatOps =
    throw new java.lang.UnsupportedOperationException
  
  implicit val Float: FloatType
  
  
  abstract class DoubleOps {
    def abs: Double = macro DoubleMacros.abs
    
    def min(that: Double): Double = macro DoubleMacros.min
    
    def max(that: Double): Double = macro DoubleMacros.max
    
    def sqrt: Double = macro DoubleMacros.sqrt
    
    def toLongBits: Long = macro DoubleMacros.toLongBits
  }
  
  class DoubleType extends Hash[Double] with Show[Double] {
    final def MinValue: Double = macro DoubleMacros.MinValue
    
    final def MaxValue: Double = macro DoubleMacros.MaxValue
    
    final def Infinity: Double = macro DoubleMacros.Infinity
    
    final def NaN: Double = macro DoubleMacros.NaN
    
    override def equal(x: Double, y: Double): Boolean = x == y
    
    override def hash(x: Double): Int = x.##
    
    override def show(x: Double)(implicit buffer: CharBuffer): buffer.State = {
      buffer.append(java.lang.Double.toString(x))
      buffer.check
    }
  }
  
  implicit def DoubleOps(value: Double): DoubleOps =
    throw new java.lang.UnsupportedOperationException
  
  implicit val Double: DoubleType
  
  
  class BooleanType extends Hash[Boolean] with Show[Boolean] {
    override def equal(x: Boolean, y: Boolean): Boolean = x == y
    
    override def hash(x: Boolean): Int = x.##
    
    override def show(x: Boolean)(implicit buffer: CharBuffer): buffer.State = {
      buffer.append(if (x) "true" else "false")
      buffer.check
    }
  }
  
  implicit val Boolean: BooleanType
}
