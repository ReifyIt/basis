/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package object basis {
  /** Returns `true` given equal values and `false` given unequal ones,
    * according to an implicit `Equal` implementation. */
  def equal[T](x: T, y: T)(implicit T: Equal[T]): Boolean =
    macro Macros.equal[T]
  
  /** Returns the hash code of the given value according to an implicit
    * `Hash` implementation. */
  def hash[T](x: T)(implicit T: Hash[T]): Int =
    macro Macros.hash[T]
  
  /** Returns a description of the given value produced by an implicit
    * `Show` implementation. */
  def show[T](x: T)(implicit T: Show[T], buffer: CharBuffer): buffer.State =
    macro Macros.show[T]
  
  /** `Byte` constants and implicit type class implementations. */
  implicit object Byte extends Hash[Byte] with Show[Byte] {
    def MinValue: Byte = macro ByteMacros.MinValue
    
    def MaxValue: Byte = macro ByteMacros.MaxValue
    
    override def equal(x: Byte, y: Byte): Boolean = x == y
    
    override def hash(x: Byte): Int = x.##
    
    override def show(x: Byte)(implicit buffer: CharBuffer): Unit = Int.show(x)
  }
  
  /** `Short` constants and implicit type class implementations. */
  implicit object Short extends Hash[Short] with Show[Short] {
    def MinValue: Short = macro ShortMacros.MinValue
    
    def MaxValue: Short = macro ShortMacros.MaxValue
    
    override def equal(x: Short, y: Short): Boolean = x == y
    
    override def hash(x: Short): Int = x.##
    
    override def show(x: Short)(implicit buffer: CharBuffer): Unit = Int.show(x)
  }
  
  /** `Int` constants and implicit type class implementations. */
  implicit object Int extends Hash[Int] with Show[Int] {
    def MinValue: Int = macro IntMacros.MinValue
    
    def MaxValue: Int = macro IntMacros.MaxValue
    
    override def equal(x: Int, y: Int): Boolean = x == y
    
    override def hash(x: Int):Int = x.##
    
    override def show(x: Int)(implicit buffer: CharBuffer) {
      if (x < 0) buffer += '-'
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
        buffer += ds(i)
        i -= 1
      }
    }
  }
  
  /** Implicitly adds supplemental operations to `Int` values. */
  implicit def IntOps(value: Int): IntOps =
    throw new java.lang.UnsupportedOperationException
  
  /** `Long` constants and implicit type class implementations. */
  implicit object Long extends Hash[Long] with Show[Long] {
    def MinValue: Long = macro LongMacros.MinValue
    
    def MaxValue: Long = macro LongMacros.MaxValue
    
    override def equal(x: Long, y: Long): Boolean = x == y
    
    override def hash(x: Long): Int = x.##
    
    override def show(x: Long)(implicit buffer: CharBuffer) {
      if (x < 0L) buffer += '-'
      var ds = new Array[Char](19)
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
        buffer += ds(i)
        i -= 1
      }
      buffer += 'L'
    }
  }
  
  /** Implicitly adds supplemental operations to `Long` values. */
  implicit def LongOps(value: Long): LongOps =
    throw new java.lang.UnsupportedOperationException
  
  /** `Float` constants and implicit type class implementations. */
  implicit object Float extends Hash[Float] with Show[Float] {
    def MinValue: Float = macro FloatMacros.MinValue
    
    def MaxValue: Float = macro FloatMacros.MaxValue
    
    def Infinity: Float = macro FloatMacros.Infinity
    
    def NaN: Float = macro FloatMacros.NaN
    
    override def equal(x: Float, y: Float): Boolean = x == y
    
    override def hash(x: Float): Int = x.##
    
    override def show(x: Float)(implicit buffer: CharBuffer): Unit =
      buffer.append(java.lang.Float.toString(x))
  }
  
  /** Implicitly adds supplemental operations to `Float` values. */
  implicit def FloatOps(value: Float): FloatOps =
    throw new java.lang.UnsupportedOperationException
  
  /** `Double` constants and implicit type class implementations. */
  implicit object Double extends Hash[Double] with Show[Double] {
    def MinValue: Double = macro DoubleMacros.MinValue
    
    def MaxValue: Double = macro DoubleMacros.MaxValue
    
    def Infinity: Double = macro DoubleMacros.Infinity
    
    def NaN: Double = macro DoubleMacros.NaN
    
    override def equal(x: Double, y: Double): Boolean = x == y
    
    override def hash(x: Double): Int = x.##
    
    override def show(x: Double)(implicit buffer: CharBuffer): Unit =
      buffer.append(java.lang.Double.toString(x))
  }
  
  /** Implicitly adds supplemental operations to `Double` values. */
  implicit def DoubleOps(value: Double): DoubleOps =
    throw new java.lang.UnsupportedOperationException
  
  /** `Boolean` constants and implicit type class implementations. */
  implicit object Boolean extends Hash[Boolean] with Show[Boolean] {
    override def equal(x: Boolean, y: Boolean): Boolean = x == y
    
    override def hash(x: Boolean): Int = x.##
    
    override def show(x: Boolean)(implicit buffer: CharBuffer): Unit =
      buffer.append(if (x) "true" else "false")
  }
  
  /** Implicit `String` type class implementations. */
  implicit object String extends Hash[String] with Show[String] {
    override def equal(s: String, t: String): Boolean = s == t
    
    override def hash(s: String): Int = s.##
    
    override def show(s: String)(implicit buffer: CharBuffer) {
      buffer += '\"'
      var i = 0
      val n = s.length
      while (i < n) {
        s.codePointAt(i) match {
          case '\b' => buffer += '\\' += 'b'
          case '\t' => buffer += '\\' += 't'
          case '\n' => buffer += '\\' += 'n'
          case '\f' => buffer += '\\' += 'f'
          case '\r' => buffer += '\\' += 'r'
          case '\"' => buffer += '\\' += '\"'
          case '\\' => buffer += '\\' += '\\'
          case codePoint => buffer += codePoint
        }
        i = s.offsetByCodePoints(i, 1)
      }
      buffer += '\"'
    }
  }
  
  /** Implicitly provides CharBuffers that build Strings. */
  implicit def StringBuffer: CharBuffer { type State = String } = new StringBuffer
}
