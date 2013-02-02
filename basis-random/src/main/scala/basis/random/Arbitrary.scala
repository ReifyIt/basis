/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.random

import basis.collections._
import basis.containers._
import basis.runtime._
import basis.util._

import scala.annotation.{implicitNotFound, tailrec, unspecialized}

/** An arbitrary value generator.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
@implicitNotFound("No arbitrary generator available for type ${R}.")
trait Arbitrary[@specialized(Specializable.Primitives) +R] extends (() => R) {
  @unspecialized def map[T](f: R => T): Arbitrary[T] = new Map(f)
  
  @unspecialized def flatMap[T](f: R => Arbitrary[T]): Arbitrary[T] = new FlatMap(f)
  
  @unspecialized def filter(p: R => Boolean): Arbitrary[R] = new Filter(1024)(p)
  
  @unspecialized def withFilter(p: R => Boolean): Arbitrary[R] = new Filter(1024)(p)
  
  override def toString: String = "<arbitrary>"
  
  private final class Map[+T](f: R => T) extends Arbitrary[T] {
    override def apply(): T = f(Arbitrary.this())
    override def toString: String = Arbitrary.this.toString +"."+"map"+"("+ f +")"
  }
  
  private final class FlatMap[+T](f: R => Arbitrary[T]) extends Arbitrary[T] {
    override def apply(): T = f(Arbitrary.this())()
    override def toString: String = Arbitrary.this.toString +"."+"flatMap"+"("+ f +")"
  }
  
  private final class Filter(TTL: Int)(p: R => Boolean) extends Arbitrary[R] {
    @tailrec private[this] def apply(ttl: Int): R = {
      if (ttl < 0) throw new ArbitraryException("improbable filter")
      val x = Arbitrary.this()
      if (p(x)) x
      else apply(ttl - 1)
    }
    override def apply(): R = apply(TTL)
    override def toString: String = Arbitrary.this.toString +"."+"filter"+"("+ p +")"
  }
}

/** A factory for [[Arbitrary]] value generators. */
object Arbitrary {
  def apply[R](implicit R: Arbitrary[R]): R.type = R
  
  def Constant[@specialized(Specializable.Primitives) R](value: R): Arbitrary[R] = new Constant(value)
  
  implicit def Byte(implicit S: Entropy): Arbitrary[Byte] = new RandomByte
  
  def PositiveByte(implicit S: Entropy): Arbitrary[Byte] = new RandomPositiveByte
  
  implicit def Short(implicit S: Entropy): Arbitrary[Short] = new RandomShort
  
  def PositiveShort(implicit S: Entropy): Arbitrary[Short] = new RandomPositiveShort
  
  implicit def Int(implicit S: Entropy): Arbitrary[Int] = new RandomInt
  
  def PositiveInt(implicit S: Entropy): Arbitrary[Int] = new RandomPositiveInt
  
  implicit def Long(implicit S: Entropy): Arbitrary[Long] = new RandomLong
  
  def PositiveLong(implicit S: Entropy): Arbitrary[Long] = new RandomPositiveLong
  
  implicit def Float(implicit S: Entropy): Arbitrary[Float] = new RandomFloat
  
  implicit def Double(implicit S: Entropy): Arbitrary[Double] = new RandomDouble
  
  implicit def Alphanumeric(implicit S: Entropy): Arbitrary[Char] = new RandomAlphanumeric
  
  def Letter(implicit S: Entropy): Arbitrary[Char] = new RandomLetter
  
  def Lowercase(implicit S: Entropy): Arbitrary[Char] = new RandomLowercase
  
  def Uppercase(implicit S: Entropy): Arbitrary[Char] = new RandomUppercase
  
  def Numeral(implicit S: Entropy): Arbitrary[Char] = new RandomNumeral
  
  implicit def Boolean(implicit S: Entropy): Arbitrary[Boolean] = new RandomBoolean
  
  implicit def Unit: Arbitrary[Unit] = RandomUnit
  
  implicit def String(implicit C: Arbitrary[Char]): Arbitrary[String] =
    new RandomString(below(64))(C)
  
  def String(length: Arbitrary[Int])(implicit C: Arbitrary[Char]): Arbitrary[String] =
    new RandomString(length)(C)
  
  implicit def Container[CC[_], A]
      (implicit CC: BuilderFactory[CC],
                A: Arbitrary[A],
                HintA: TypeHint[A])
    : Arbitrary[CC[A]] =
    new RandomContainer(below(64))(CC, A, HintA)
  
  def Container[CC[_], A]
      (length: Arbitrary[Int])
      (implicit CC: BuilderFactory[CC],
                A: Arbitrary[A],
                HintA: TypeHint[A])
    : Arbitrary[CC[A]] =
    new RandomContainer(length)(CC, A, HintA)
  
  implicit def Map[CC[_, _], A, T]
      (implicit CC: MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T],
                HintA: TypeHint[A],
                HintT: TypeHint[T])
    : Arbitrary[CC[A, T]] =
    new RandomMap(below(64))(CC, A, T, HintA, HintT)
  
  def Map[CC[_, _], A, T]
      (length: Arbitrary[Int])
      (implicit CC: MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T],
                HintA: TypeHint[A],
                HintT: TypeHint[T])
    : Arbitrary[CC[A, T]] =
    new RandomMap(length)(CC, A, T, HintA, HintT)
  
  implicit def Tuple2[T1, T2]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2])
    : Arbitrary[(T1, T2)] = new RandomTuple2
  
  implicit def Tuple3[T1, T2, T3]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3])
    : Arbitrary[(T1, T2, T3)] = new RandomTuple3
  
  implicit def Tuple4[T1, T2, T3, T4]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3],
                T4: Arbitrary[T4])
    : Arbitrary[(T1, T2, T3, T4)] = new RandomTuple4
  
  implicit def apply[T1, R]
      (f: T1 => R)
      (implicit T1: Arbitrary[T1])
    : Arbitrary[R] = new RandomFunction1(f)
  
  implicit def apply[T1, T2, R]
      (f: (T1, T2) => R)
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2])
    : Arbitrary[R] = new RandomFunction2(f)
  
  implicit def apply[T1, T2, T3, R]
      (f: (T1, T2, T3) => R)
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3])
    : Arbitrary[R] = new RandomFunction3(f)
  
  implicit def apply[T1, T2, T3, T4, R]
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3],
                T4: Arbitrary[T4])
    : Arbitrary[R] = new RandomFunction4(f)
  
  def below(upper: Byte)(implicit S: Entropy): Arbitrary[Byte] = new RandomByteBelow(upper)
  
  def below(upper: Short)(implicit S: Entropy): Arbitrary[Short] = new RandomShortBelow(upper)
  
  def below(upper: Int)(implicit S: Entropy): Arbitrary[Int] = new RandomIntBelow(upper)
  
  def below(upper: Long)(implicit S: Entropy): Arbitrary[Long] = new RandomLongBelow(upper)
  
  def below(upper: Float)(implicit S: Entropy): Arbitrary[Float] = new RandomFloatBelow(upper)
  
  def below(upper: Double)(implicit S: Entropy): Arbitrary[Double] = new RandomDoubleBelow(upper)
  
  def between(lower: Byte, upper: Byte)(implicit S: Entropy): Arbitrary[Byte] = new RandomByteBetween(lower, upper)
  
  def between(lower: Short, upper: Short)(implicit S: Entropy): Arbitrary[Short] = new RandomShortBetween(lower, upper)
  
  def between(lower: Int, upper: Int)(implicit S: Entropy): Arbitrary[Int] = new RandomIntBetween(lower, upper)
  
  def between(lower: Long, upper: Long)(implicit S: Entropy): Arbitrary[Long] = new RandomLongBetween(lower, upper)
  
  def between(lower: Float, upper: Float)(implicit S: Entropy): Arbitrary[Float] = new RandomFloatBetween(lower, upper)
  
  def between(lower: Double, upper: Double)(implicit S: Entropy): Arbitrary[Double] = new RandomDoubleBetween(lower, upper)
  
  def choose[A](count: Int, elems: Enumerator[A])(implicit S: Entropy): Arbitrary[List[A]] = new Choose(count, elems)
  
  def pick[A](elems: Enumerator[A])(implicit S: Entropy): Arbitrary[A] = new Pick(elems)
  
  private final class Constant[@specialized(Specializable.Primitives) +R](value: R) extends Arbitrary[R] {
    override def apply(): R = value
    override def toString: String = value.toString
  }
  
  private final class RandomByte(implicit S: Entropy) extends Arbitrary[Byte] {
    override def apply(): Byte = S.nextByte()
    override def toString: String = "Byte"
  }
  
  private final class RandomPositiveByte(implicit S: Entropy) extends Arbitrary[Byte] {
    override def apply(): Byte = (S.nextByte() >>> 1).toByte
    override def toString: String = "PositiveByte"
  }
  
  private final class RandomByteBelow(upper: Byte)(implicit S: Entropy) extends Arbitrary[Byte] {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Byte = (S.nextInt() % upper).abs.toByte
    override def toString: String = "below"+"("+ upper +"."+"toByte"+")"
  }
  
  private final class RandomByteBetween(lower: Byte, upper: Byte)(implicit S: Entropy) extends Arbitrary[Byte] {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Byte = (lower + (S.nextInt() % size).abs).toByte
    override def toString: String = "between"+"("+ lower +"."+"toByte"+", "+ upper +"."+"toByte"+")"
  }
  
  private final class RandomShort(implicit S: Entropy) extends Arbitrary[Short] {
    override def apply(): Short = S.nextShort()
    override def toString: String = "Short"
  }
  
  private final class RandomPositiveShort(implicit S: Entropy) extends Arbitrary[Short] {
    override def apply(): Short = (S.nextShort() >>> 1).toShort
    override def toString: String = "PositiveShort"
  }
  
  private final class RandomShortBelow(upper: Short)(implicit S: Entropy) extends Arbitrary[Short] {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Short = (S.nextInt() % upper).abs.toShort
    override def toString: String = "below"+"("+ upper +"."+"toShort"+")"
  }
  
  private final class RandomShortBetween(lower: Short, upper: Short)(implicit S: Entropy) extends Arbitrary[Short] {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Short = (lower + (S.nextInt() % size).abs).toShort
    override def toString: String = "between"+"("+ lower +"."+"toShort"+", "+ upper +"."+"toShort"+")"
  }
  
  private final class RandomInt(implicit S: Entropy) extends Arbitrary[Int] {
    override def apply(): Int = S.nextInt()
    override def toString: String = "Int"
  }
  
  private final class RandomPositiveInt(implicit S: Entropy) extends Arbitrary[Int] {
    override def apply(): Int = S.nextInt() >>> 1
    override def toString: String = "PositiveInt"
  }
  
  private final class RandomIntBelow(upper: Int)(implicit S: Entropy) extends Arbitrary[Int] {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Int = (S.nextInt() % upper).abs
    override def toString: String = "below"+"("+ upper +")"
  }
  
  private final class RandomIntBetween(lower: Int, upper: Int)(implicit S: Entropy) extends Arbitrary[Int] {
    private[this] val size: Int = (upper - lower) + 1
    if (upper < lower || size < 0) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    override def apply(): Int = lower + (S.nextInt() % size).abs
    override def toString: String = "between"+"("+ lower +", "+ upper +")"
  }
  
  private final class RandomLong(implicit S: Entropy) extends Arbitrary[Long] {
    override def apply(): Long = S.nextLong()
    override def toString: String = "Long"
  }
  
  private final class RandomPositiveLong(implicit S: Entropy) extends Arbitrary[Long] {
    override def apply(): Long = S.nextLong() >>> 1
    override def toString: String = "PositiveLong"
  }
  
  private final class RandomLongBelow(upper: Long)(implicit S: Entropy) extends Arbitrary[Long] {
    if (upper < 0L) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Long = (S.nextLong() % upper).abs
    override def toString: String = "below"+"("+ upper +"L"+")"
  }
  
  private final class RandomLongBetween(lower: Long, upper: Long)(implicit S: Entropy) extends Arbitrary[Long] {
    private[this] val size: Long = (upper - lower) + 1L
    if (upper < lower || size < 0L) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    override def apply(): Long = lower + (S.nextLong() % size).abs
    override def toString: String = "between"+"("+ lower +"L"+", "+ upper +"L"+")"
  }
  
  private final class RandomFloat(implicit S: Entropy) extends Arbitrary[Float] {
    override def apply(): Float = S.nextFloat()
    override def toString: String = "Float"
  }
  
  private final class RandomFloatBelow(upper: Float)(implicit S: Entropy) extends Arbitrary[Float] {
    if (upper.isNaN || upper.isInfinite || upper < 0.0F)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Float = S.nextFloat() * upper
    override def toString: String = "below"+"("+ upper +"f"+")"
  }
  
  private final class RandomFloatBetween(lower: Float, upper: Float)(implicit S: Entropy) extends Arbitrary[Float] {
    private[this] val size: Float = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    override def apply(): Float = lower + S.nextFloat() * size
    override def toString: String = "between"+"("+ lower +"f"+", "+ upper +"f"+")"
  }
  
  private final class RandomDouble(implicit S: Entropy) extends Arbitrary[Double] {
    override def apply(): Double = S.nextDouble()
    override def toString: String = "Double"
  }
  
  private final class RandomDoubleBelow(upper: Double)(implicit S: Entropy) extends Arbitrary[Double] {
    if (upper.isNaN || upper.isInfinite || upper < 0.0)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Double = S.nextDouble() * upper
    override def toString: String = "below"+"("+ upper +")"
  }
  
  private final class RandomDoubleBetween(lower: Double, upper: Double)(implicit S: Entropy) extends Arbitrary[Double] {
    private[this] val size: Double = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    override def apply(): Double = lower + S.nextDouble() * size
    override def toString: String = "between"+"("+ lower +", "+ upper +")"
  }
  
  private final class RandomAlphanumeric(implicit S: Entropy) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBelow(10)(S)
    private[this] val lowercase = new RandomLowercase()(S)
    private[this] val uppercase = new RandomUppercase()(S)
    private[this] val numeral = new RandomNumeral()(S)
    override def apply(): Char = roll() match {
      case 0 => numeral()
      case 1 => uppercase()
      case _ => lowercase()
    }
    override def toString: String = "Alphanumeric"
  }
  
  private final class RandomLetter(implicit S: Entropy) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBelow(5)(S)
    private[this] val lowercase = new RandomLowercase()(S)
    private[this] val uppercase = new RandomUppercase()(S)
    override def apply(): Char = if (roll() == 0) uppercase() else lowercase()
    override def toString: String = "Letter"
  }
  
  private final class RandomLowercase(implicit S: Entropy) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBetween(97, 122)(S)
    override def apply(): Char = roll().toChar
    override def toString: String = "Lowercase"
  }
  
  private final class RandomUppercase(implicit S: Entropy) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBetween(65, 90)(S)
    override def apply(): Char = roll().toChar
    override def toString: String = "Uppercase"
  }
  
  private final class RandomNumeral(implicit S: Entropy) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBetween(48, 57)(S)
    override def apply(): Char = roll().toChar
    override def toString: String = "Numeral"
  }
  
  private final class RandomBoolean(implicit S: Entropy) extends Arbitrary[Boolean] {
    override def apply(): Boolean = S.nextBoolean()
    override def toString: String = "Boolean"
  }
  
  private object RandomUnit extends Arbitrary[Unit] {
    override def apply(): Unit = ()
    override def toString: String = "Unit"
  }
  
  private final class RandomString(length: Arbitrary[Int])(implicit C: Arbitrary[Char]) extends Arbitrary[String] {
    override def apply(): String = {
      var i = 0
      val n = length()
      val s = new java.lang.StringBuilder
      s.ensureCapacity(n)
      while (i < n) {
        s.append(C())
        i += 1
      }
      s.toString
    }
    
    override def toString: String = "String"+"("+ C +")"
  }
  
  private final class RandomContainer[CC[_], A]
      (length: Arbitrary[Int])
      (implicit CC: BuilderFactory[CC],
                A: Arbitrary[A],
                HintA: TypeHint[A])
    extends Arbitrary[CC[A]] {
    
    override def apply(): CC[A] = {
      var i = 0
      val n = length()
      val b = CC.Builder(HintA).expect(n)
      while (i < n) {
        b.append(A())
        i += 1
      }
      b.state
    }
    
    override def toString: String = "Container"+"("+ CC +", "+ A +")"
  }
  
  private final class RandomMap[CC[_, _], A, T]
      (length: Arbitrary[Int])
      (implicit CC: MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T],
                HintA: TypeHint[A],
                HintT: TypeHint[T])
    extends Arbitrary[CC[A, T]] {
    
    override def apply(): CC[A, T] = {
      var i = 0
      val n = length()
      val b = CC.Builder(HintA, HintT).expect(n)
      while (i < n) {
        b.append((A(), T()))
        i += 1
      }
      b.state
    }
    
    override def toString: String = "Map"+"("+ CC +", "+ A +", "+ T +")"
  }
  
  private final class RandomTuple2[+T1, +T2]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2])
    extends Arbitrary[(T1, T2)] {
    override def apply() = (T1(), T2())
    override def toString: String = s"($T1, $T2)"
  }
  
  private final class RandomTuple3[+T1, +T2, +T3]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3])
    extends Arbitrary[(T1, T2, T3)] {
    override def apply() = (T1(), T2(), T3())
    override def toString: String = s"($T1, $T2, $T3)"
  }
  
  private final class RandomTuple4[+T1, +T2, +T3, +T4]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3],
                T4: Arbitrary[T4])
    extends Arbitrary[(T1, T2, T3, T4)] {
    override def apply() = (T1(), T2(), T3(), T4())
    override def toString: String = s"($T1, $T2, $T3, $T4)"
  }
  
  private final class RandomFunction1[-T1, +R]
      (f: T1 => R)
      (implicit T1: Arbitrary[T1])
    extends Arbitrary[R] {
    override def apply(): R = f(T1())
    override def toString: String = "<arbitrary1>"
  }
  
  private final class RandomFunction2[-T1, -T2, +R]
      (f: (T1, T2) => R)
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2])
    extends Arbitrary[R] {
    override def apply(): R = f(T1(), T2())
    override def toString: String = "<arbitrary2>"
  }
  
  private final class RandomFunction3[-T1, -T2, -T3, +R]
      (f: (T1, T2, T3) => R)
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3])
    extends Arbitrary[R] {
    override def apply(): R = f(T1(), T2(), T3())
    override def toString: String = "<arbitrary3>"
  }
  
  private final class RandomFunction4[-T1, -T2, -T3, -T4, +R]
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3],
                T4: Arbitrary[T4])
    extends Arbitrary[R] {
    override def apply(): R = f(T1(), T2(), T3(), T4())
    override def toString: String = "<arbitrary4>"
  }
  
  private final class Choose[+A]
      (count: Int, elems: ListBuffer[A])
      (implicit S: Entropy)
    extends Arbitrary[List[A]] {
    
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (count > elems.length) throw new IllegalArgumentException("count > length")
    
    def this(count: Int, elems: Enumerator[A])(implicit S: Entropy) =
      this(count, new ListBuffer[A] ++= elems)(S)
    
    override def apply(): List[A] = {
      val xs = elems.copy
      while (xs.length > count) xs.remove(S.nextBelow(xs.length))
      xs.toList
    }
    
    override def toString: String = "<choose>"
  }
  
  private final class Pick[+A](elems: Index[A])(implicit S: Entropy) extends Arbitrary[A] {
    if (elems.length == 0) throw new IllegalArgumentException("empty pick")
    
    private[this] val index = new RandomIntBelow(elems.length)(S)
    
    def this(elems: Enumerator[A])(implicit S: Entropy) =
      this((ArraySeq.Builder[A] ++= elems).state)(S)
    
    override def apply(): A = elems(index())
    
    override def toString: String = "<pick>"
  }
}
