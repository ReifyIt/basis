/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.util

/** Implements Austin Appleby's MurmurHash 3 algorithm,
  * specifically MurmurHash3_x86_32 revision 136.
  * 
  * @example {{{
  * import MurmurHash3._
  * mash(mix(mix(mix(seed[Example], hash(x)), hash(y)), hash(z)))
  * }}}
  * 
  * @see  [[http://code.google.com/p/smhasher/]]
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * 
  * @groupprio  Mixing    1
  * @groupprio  Hashing   2
  * @groupprio  Seeding   3
  */
object MurmurHash3 {
  import java.lang.Integer.{rotateLeft => rotl}
  import java.lang.Float.floatToIntBits
  import java.lang.Double.doubleToLongBits
  
  /** Returns a literal hash of the full name of a type symbol
    * for use as a seed value.
    * @group Seeding */
  def seed[T]: Int = macro MurmurHash3Macros.seedType[T]
  
  /** Returns a literal hash of a compile-time constant `String`
    * for use as a seed value.
    * @group Seeding */
  def seed(string: String): Int = macro MurmurHash3Macros.seedString
  
  /** Returns a hash of a `Byte` value.
    * @group Hashing */
  def hash(x: Byte): Int = x.toInt
  
  /** Returns a hash of a `Short` value.
    * @group Hashing */
  def hash(x: Short): Int = x.toInt
  
  /** Returns a hash of an `Int` value.
    * @group Hashing */
  def hash(x: Int): Int = x
  
  /** Returns a hash of a `Long` value. 
    * @group Hashing */
  def hash(x: Long): Int = x.toInt ^ ((x >>> 32).toInt + (x >>> 63).toInt)
  
  /** Returns a hash of a `Float` value.
    * @group Hashing */
  def hash(x: Float): Int = {
    if (x == x.toInt.toFloat) x.toInt
    else if (x == x.toLong.toFloat) hash(x.toLong)
    else floatToIntBits(x)
  }
  
  /** Returns a hash of a `Double` value.
    * @group Hashing */
  def hash(x: Double): Int = {
    if (x == x.toInt.toDouble) x.toInt
    else if (x == x.toLong.toDouble) hash(x.toLong)
    else if (x == x.toFloat.toDouble) floatToIntBits(x.toFloat)
    else {
      val y = doubleToLongBits(x)
      y.toInt ^ (y >>> 32).toInt
    }
  }
  
  /** Returns a hash of a `Char` value.
    * @group Hashing */
  def hash(x: Char): Int = x.toInt
  
  /** Returns a hash of a `Boolean` value.
    * @group Hashing */
  def hash(x: Boolean): Int = {
    if (x) java.lang.Boolean.TRUE.hashCode
    else java.lang.Boolean.FALSE.hashCode
  }
  
  /** Returns a hash of `()`.
    * @group Hashing */
  def hash(x: Unit): Int = 0
  
  /** Returns a hash of a `String`.
    * @group Hashing */
  def hash(x: String): Int = {
    var h = 0
    var i = 0
    val n = x.length
    while (i < n) {
      h = mix(h, x.codePointAt(i))
      i = x.offsetByCodePoints(i, 1)
    }
    mash(h)
  }
  
  /** Returns a hash of a `Number`.
    * @group Hashing */
  private def hash(x: java.lang.Number): Int = {
    if (x.isInstanceOf[java.lang.Double]) hash(x.doubleValue)
    else if (x.isInstanceOf[java.lang.Float]) hash(x.floatValue)
    else if (x.isInstanceOf[java.lang.Long]) hash(x.longValue)
    else x.intValue
  }
  
  /** Returns a hash of any value.
    * @group Hashing */
  def hash(x: Any): Int = {
    if (x == null) 0
    else if (x.isInstanceOf[java.lang.Number])
      hash(x.asInstanceOf[java.lang.Number])
    else x.hashCode
  }
  
  /** Returns a hash code mixed with a new hash value.
    * @group Mixing */
  def mix(code: Int, value: Int): Int = {
    var h = code
    var k = value
    
    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593
    
    h ^= k
    
    h = rotl(h, 13)
    h = h * 5 + 0xe6546b64
    
    h
  }
  
  /** Returns a finalized hash code.
    * @group Mixing */
  def mash(code: Int): Int = {
    var h = code
    
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    
    h
  }
}

private[util] object MurmurHash3Macros {
  import scala.reflect.macros.Context
  
  def seedString(c: Context)(string: c.Expr[String]): c.Expr[Int] = {
    import c.universe._
    c.literal(MurmurHash3.hash(string.tree match {
      case Literal(Constant(s: String)) => s
      case t => c.abort(t.pos, "Seed string not a compile-time constant.")
    }))
  }
  
  def seedType[T](c: Context)(implicit T: c.WeakTypeTag[T]): c.Expr[Int] =
    c.literal(MurmurHash3.hash(T.tpe.typeSymbol.fullName))
}
