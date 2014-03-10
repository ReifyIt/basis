//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.stat

import basis.collections._
import basis.util._
import scala.annotation._

/** An arbitrary value generator.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
@implicitNotFound("No arbitrary generator available for type ${A}.")
trait Arbitrary[@specialized(Arbitrary.Specialized) +A] extends (() => A) {
  @unspecialized def map[B](f: A => B): Arbitrary[B] = new Map(f)

  @unspecialized def flatMap[B](f: A => Arbitrary[B]): Arbitrary[B] = new FlatMap(f)

  @unspecialized def filter(p: A => Boolean): Arbitrary[A] = new Filter(1024)(p)

  @unspecialized def withFilter(p: A => Boolean): Arbitrary[A] = new Filter(1024)(p)

  override def toString: String = "<arbitrary>"

  private final class Map[+B](f: A => B) extends Arbitrary[B] {
    override def apply(): B = f(Arbitrary.this())
    override def toString: String = Arbitrary.this.toString +"."+"map"+"("+ f +")"
  }

  private final class FlatMap[+B](f: A => Arbitrary[B]) extends Arbitrary[B] {
    override def apply(): B = f(Arbitrary.this())()
    override def toString: String = Arbitrary.this.toString +"."+"flatMap"+"("+ f +")"
  }

  private final class Filter(TTL: Int)(p: A => Boolean) extends Arbitrary[A] {
    @tailrec private[this] def apply(ttl: Int): A = {
      if (ttl < 0) throw new ArbitraryException("improbable filter")
      val x = Arbitrary.this()
      if (p(x)) x
      else apply(ttl - 1)
    }
    override def apply(): A = apply(TTL)
    override def toString: String = Arbitrary.this.toString +"."+"filter"+"("+ p +")"
  }
}

/** Contains builtin [[Arbitrary]] value generators. */
object Arbitrary {
  protected final val Specialized = new Specializable.Group((scala.Byte, scala.Short, scala.Int, scala.Long, scala.Char, scala.Float, scala.Double, scala.Boolean))

  def apply[A](implicit A: Arbitrary[A]): A.type = A

  def Constant[@specialized(Specialized) A](value: A): Arbitrary[A] = new Constant(value)

  implicit def Byte(implicit S: Random): Arbitrary[Byte] = new RandomByte(S)

  def PositiveByte(implicit S: Random): Arbitrary[Byte] = new RandomPositiveByte(S)

  def ByteBelow(upper: Byte)(implicit S: Random): Arbitrary[Byte] = new RandomByteBelow(upper)(S)

  def ByteBetween(lower: Byte, upper: Byte)(implicit S: Random): Arbitrary[Byte] = new RandomByteBetween(lower, upper)(S)

  implicit def Short(implicit S: Random): Arbitrary[Short] = new RandomShort(S)

  def PositiveShort(implicit S: Random): Arbitrary[Short] = new RandomPositiveShort(S)

  def ShortBelow(upper: Short)(implicit S: Random): Arbitrary[Short] = new RandomShortBelow(upper)(S)

  def ShortBetween(lower: Short, upper: Short)(implicit S: Random): Arbitrary[Short] = new RandomShortBetween(lower, upper)(S)

  implicit def Int(implicit S: Random): Arbitrary[Int] = new RandomInt(S)

  def PositiveInt(implicit S: Random): Arbitrary[Int] = new RandomPositiveInt(S)

  def IntBelow(upper: Int)(implicit S: Random): Arbitrary[Int] = new RandomIntBelow(upper)(S)

  def IntBetween(lower: Int, upper: Int)(implicit S: Random): Arbitrary[Int] = new RandomIntBetween(lower, upper)(S)

  implicit def Long(implicit S: Random): Arbitrary[Long] = new RandomLong(S)

  def PositiveLong(implicit S: Random): Arbitrary[Long] = new RandomPositiveLong(S)

  def LongBelow(upper: Long)(implicit S: Random): Arbitrary[Long] = new RandomLongBelow(upper)(S)

  def LongBetween(lower: Long, upper: Long)(implicit S: Random): Arbitrary[Long] = new RandomLongBetween(lower, upper)(S)

  implicit def Float(implicit S: Random): Arbitrary[Float] = new RandomFloat(S)

  def FloatBelow(upper: Float)(implicit S: Random): Arbitrary[Float] = new RandomFloatBelow(upper)(S)

  def FloatBetween(lower: Float, upper: Float)(implicit S: Random): Arbitrary[Float] = new RandomFloatBetween(lower, upper)(S)

  implicit def Double(implicit S: Random): Arbitrary[Double] = new RandomDouble(S)

  def DoubleBelow(upper: Double)(implicit S: Random): Arbitrary[Double] = new RandomDoubleBelow(upper)(S)

  def DoubleBetween(lower: Double, upper: Double)(implicit S: Random): Arbitrary[Double] = new RandomDoubleBetween(lower, upper)(S)

  implicit def Boolean(implicit S: Random): Arbitrary[Boolean] = new RandomBoolean(S)

  implicit def Unit: Arbitrary[Unit] = RandomUnit

  def Lowercase(implicit S: Random): Arbitrary[Char] = new RandomLowercase(S)

  def Uppercase(implicit S: Random): Arbitrary[Char] = new RandomUppercase(S)

  def Letter(implicit S: Random): Arbitrary[Char] = new RandomLetter(S)

  def Numeral(implicit S: Random): Arbitrary[Char] = new RandomNumeral(S)

  implicit def Alphanumeric(implicit S: Random): Arbitrary[Char] = new RandomAlphanumeric(S)

  implicit def String(implicit C: Arbitrary[Char]): Arbitrary[String] =
    new RandomString(IntBelow(64))(C)

  def String(length: Arbitrary[Int])(implicit C: Arbitrary[Char]): Arbitrary[String] =
    new RandomString(length)(C)

  implicit def Collection[CC[_], A]
      (implicit CC: generic.CollectionFactory[CC],
                A: Arbitrary[A])
    : Arbitrary[CC[A]] =
    new RandomCollection(IntBelow(64))(CC, A)

  def Collection[CC[_], A]
      (length: Arbitrary[Int])
      (implicit CC: generic.CollectionFactory[CC],
                A: Arbitrary[A])
    : Arbitrary[CC[A]] =
    new RandomCollection(length)(CC, A)

  implicit def Map[CC[_, _], A, T]
      (implicit CC: generic.MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T])
    : Arbitrary[CC[A, T]] =
    new RandomMap(IntBelow(64))(CC, A, T)

  def Map[CC[_, _], A, T]
      (length: Arbitrary[Int])
      (implicit CC: generic.MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T])
    : Arbitrary[CC[A, T]] =
    new RandomMap(length)(CC, A, T)

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

  def pick[A](elems: Traverser[A])(implicit S: Random): Arbitrary[A] = new Pick(elems)(S)

  def choose[A](count: Int, elems: Traverser[A])(implicit S: Random): Arbitrary[immutable.List[A]] = new Choose(count, elems)(S)

  private final class Constant[@specialized(Specialized) +A](value: A) extends Arbitrary[A] {
    override def apply(): A = value
    override def toString: String = "Arbitrary"+"."+"Constant"+"("+ value +")"
  }

  private final class RandomByte(S: Random) extends Arbitrary[Byte] {
    override def apply(): Byte = S.randomByte()
    override def toString: String = "Arbitrary"+"."+"Byte"
  }

  private final class RandomPositiveByte(S: Random) extends Arbitrary[Byte] {
    override def apply(): Byte = (S.randomByte() >>> 1).toByte
    override def toString: String = "Arbitrary"+"."+"PositiveByte"
  }

  private final class RandomByteBelow(upper: Byte)(S: Random) extends Arbitrary[Byte] {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Byte = (S.randomInt() % upper).abs.toByte
    override def toString: String = "Arbitrary"+"."+"ByteBelow"+"("+ upper +"."+"toByte"+")"
  }

  private final class RandomByteBetween(lower: Byte, upper: Byte)(S: Random) extends Arbitrary[Byte] {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Byte = (lower + (S.randomInt() % size).abs).toByte
    override def toString: String = "Arbitrary"+"."+"ByteBetween"+"("+ lower +"."+"toByte"+", "+ upper +"."+"toByte"+")"
  }

  private final class RandomShort(S: Random) extends Arbitrary[Short] {
    override def apply(): Short = S.randomShort()
    override def toString: String = "Arbitrary"+"."+"Short"
  }

  private final class RandomPositiveShort(S: Random) extends Arbitrary[Short] {
    override def apply(): Short = (S.randomShort() >>> 1).toShort
    override def toString: String = "Arbitrary"+"."+"PositiveShort"
  }

  private final class RandomShortBelow(upper: Short)(S: Random) extends Arbitrary[Short] {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Short = (S.randomInt() % upper).abs.toShort
    override def toString: String = "Arbitrary"+"."+"ShortBelow"+"("+ upper +"."+"toShort"+")"
  }

  private final class RandomShortBetween(lower: Short, upper: Short)(S: Random) extends Arbitrary[Short] {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Short = (lower + (S.randomInt() % size).abs).toShort
    override def toString: String = "Arbitrary"+"."+"ShortBetween"+"("+ lower +"."+"toShort"+", "+ upper +"."+"toShort"+")"
  }

  private final class RandomInt(S: Random) extends Arbitrary[Int] {
    override def apply(): Int = S.randomInt()
    override def toString: String = "Arbitrary"+"."+"Int"
  }

  private final class RandomPositiveInt(S: Random) extends Arbitrary[Int] {
    override def apply(): Int = S.randomInt() >>> 1
    override def toString: String = "Arbitrary"+"."+"PositiveInt"
  }

  private final class RandomIntBelow(upper: Int)(S: Random) extends Arbitrary[Int] {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Int = (S.randomInt() % upper).abs
    override def toString: String = "Arbitrary"+"."+"IntBelow"+"("+ upper +")"
  }

  private final class RandomIntBetween(lower: Int, upper: Int)(S: Random) extends Arbitrary[Int] {
    private[this] val size: Int = (upper - lower) + 1
    if (upper < lower || size < 0) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    override def apply(): Int = lower + (S.randomInt() % size).abs
    override def toString: String = "Arbitrary"+"."+"IntBetween"+"("+ lower +", "+ upper +")"
  }

  private final class RandomLong(S: Random) extends Arbitrary[Long] {
    override def apply(): Long = S.randomLong()
    override def toString: String = "Arbitrary"+"."+"Long"
  }

  private final class RandomPositiveLong(S: Random) extends Arbitrary[Long] {
    override def apply(): Long = S.randomLong() >>> 1
    override def toString: String = "Arbitrary"+"."+"PositiveLong"
  }

  private final class RandomLongBelow(upper: Long)(S: Random) extends Arbitrary[Long] {
    if (upper < 0L) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Long = (S.randomLong() % upper).abs
    override def toString: String = "Arbitrary"+"."+"LongBelow"+"("+ upper +"L"+")"
  }

  private final class RandomLongBetween(lower: Long, upper: Long)(S: Random) extends Arbitrary[Long] {
    private[this] val size: Long = (upper - lower) + 1L
    if (upper < lower || size < 0L) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    override def apply(): Long = lower + (S.randomLong() % size).abs
    override def toString: String = "Arbitrary"+"."+"LongBetween"+"("+ lower +"L"+", "+ upper +"L"+")"
  }

  private final class RandomFloat(S: Random) extends Arbitrary[Float] {
    override def apply(): Float = S.randomFloat()
    override def toString: String = "Arbitrary"+"."+"Float"
  }

  private final class RandomFloatBelow(upper: Float)(S: Random) extends Arbitrary[Float] {
    if (upper.isNaN || upper.isInfinite || upper < 0.0F)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Float = S.randomFloat() * upper
    override def toString: String = "Arbitrary"+"."+"FloatBelow"+"("+ upper +"f"+")"
  }

  private final class RandomFloatBetween(lower: Float, upper: Float)(S: Random) extends Arbitrary[Float] {
    private[this] val size: Float = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    override def apply(): Float = lower + S.randomFloat() * size
    override def toString: String = "Arbitrary"+"."+"FloatBetween"+"("+ lower +"f"+", "+ upper +"f"+")"
  }

  private final class RandomDouble(S: Random) extends Arbitrary[Double] {
    override def apply(): Double = S.randomDouble()
    override def toString: String = "Arbitrary"+"."+"Double"
  }

  private final class RandomDoubleBelow(upper: Double)(S: Random) extends Arbitrary[Double] {
    if (upper.isNaN || upper.isInfinite || upper < 0.0)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    override def apply(): Double = S.randomDouble() * upper
    override def toString: String = "Arbitrary"+"."+"DoubleBelow"+"("+ upper +")"
  }

  private final class RandomDoubleBetween(lower: Double, upper: Double)(S: Random) extends Arbitrary[Double] {
    private[this] val size: Double = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    override def apply(): Double = lower + S.randomDouble() * size
    override def toString: String = "Arbitrary"+"."+"DoubleBetween"+"("+ lower +", "+ upper +")"
  }

  private final class RandomBoolean(S: Random) extends Arbitrary[Boolean] {
    override def apply(): Boolean = S.randomBoolean()
    override def toString: String = "Arbitrary"+"."+"Boolean"
  }

  private object RandomUnit extends Arbitrary[Unit] {
    override def apply(): Unit = ()
    override def toString: String = "Arbitrary"+"."+"Unit"
  }

  private final class RandomLowercase(S: Random) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBetween(97, 122)(S)
    override def apply(): Char = roll().toChar
    override def toString: String = "Arbitrary"+"."+"Lowercase"
  }

  private final class RandomUppercase(S: Random) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBetween(65, 90)(S)
    override def apply(): Char = roll().toChar
    override def toString: String = "Arbitrary"+"."+"Uppercase"
  }

  private final class RandomLetter(S: Random) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBelow(52)(S)
    override def apply(): Char = (roll() match {
      case x if x < 26 => x + 'A'
      case x => x + ('a' - 26)
    }).toChar
    override def toString: String = "Arbitrary"+"."+"Letter"
  }

  private final class RandomNumeral(S: Random) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBetween(48, 57)(S)
    override def apply(): Char = roll().toChar
    override def toString: String = "Arbitrary"+"."+"Numeral"
  }

  private final class RandomAlphanumeric(S: Random) extends Arbitrary[Char] {
    private[this] val roll = new RandomIntBelow(62)(S)
    override def apply(): Char = (roll() match {
      case x if x < 26 => x + 'A'
      case x if x < 52 => x + ('a' - 26)
      case x => x + ('0' - 52)
    }).toChar
    override def toString: String = "Arbitrary"+"."+"Alphanumeric"
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

    override def toString: String = "Arbitrary"+"."+"String"+"("+ C +")"
  }

  private final class RandomCollection[CC[_], A]
      (length: Arbitrary[Int])
      (implicit CC: generic.CollectionFactory[CC],
                A: Arbitrary[A])
    extends Arbitrary[CC[A]] {

    override def apply(): CC[A] = {
      var i = 0
      val n = length()
      val b = CC.Builder[A].expect(n)
      while (i < n) {
        b.append(A())
        i += 1
      }
      b.state
    }

    override def toString: String = "Arbitrary"+"."+"Collection"+"("+ CC +", "+ A +")"
  }

  private final class RandomMap[CC[_, _], A, T]
      (length: Arbitrary[Int])
      (implicit CC: generic.MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T])
    extends Arbitrary[CC[A, T]] {

    override def apply(): CC[A, T] = {
      var i = 0
      val n = length()
      val b = CC.Builder[A, T].expect(n)
      while (i < n) {
        b.append((A(), T()))
        i += 1
      }
      b.state
    }

    override def toString: String = "Arbitrary"+"."+"Map"+"("+ CC +", "+ A +", "+ T +")"
  }

  private final class RandomTuple2[+T1, +T2]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2])
    extends Arbitrary[(T1, T2)] {
    override def apply(): (T1, T2) = (T1(), T2())
    override def toString: String = "Arbitrary"+"."+"Tuple2"+"("+ T1 +", "+ T2 +")"
  }

  private final class RandomTuple3[+T1, +T2, +T3]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3])
    extends Arbitrary[(T1, T2, T3)] {
    override def apply(): (T1, T2, T3) = (T1(), T2(), T3())
    override def toString: String = "Arbitrary"+"."+"Tuple2"+"("+ T1 +", "+ T2 +", "+ T3 +")"
  }

  private final class RandomTuple4[+T1, +T2, +T3, +T4]
      (implicit T1: Arbitrary[T1],
                T2: Arbitrary[T2],
                T3: Arbitrary[T3],
                T4: Arbitrary[T4])
    extends Arbitrary[(T1, T2, T3, T4)] {
    override def apply(): (T1, T2, T3, T4) = (T1(), T2(), T3(), T4())
    override def toString: String = "Arbitrary"+"."+"Tuple2"+"("+ T1 +", "+ T2 +", "+ T3 +", "+ T4 +")"
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

  private final class Pick[+A](elems: IndexedSeq[A])(S: Random) extends Arbitrary[A] {
    def this(elems: Traverser[A])(S: Random) =
      this(immutable.ArraySeq.from(elems.asInstanceOf[Traverser[AnyRef]]).asInstanceOf[immutable.ArraySeq[A]])(S)

    if (elems.length == 0) throw new IllegalArgumentException("empty pick")

    private[this] val index = new RandomIntBelow(elems.length)(S)

    override def apply(): A = elems(index())

    override def toString: String = "Arbitrary"+"."+"Pick"+"("+ elems +")"
  }

  private final class Choose[+A]
      (count: Int, elems: mutable.ListBuffer[A])
      (S: Random)
    extends Arbitrary[immutable.List[A]] {

    def this(count: Int, elems: Traverser[A])(S: Random) =
      this(count, mutable.ListBuffer.from(elems))(S)

    if (count < 0) throw new IllegalArgumentException("negative count")
    if (count > elems.length) throw new IllegalArgumentException("count greater than collection length")

    private[this] val length: Int = elems.length

    override def apply(): immutable.List[A] = {
      val xs = elems.copy
      var n = length
      while (n > count) {
        xs.remove(S.randomIntBelow(n))
        n -= 1
      }
      xs.toList
    }

    override def toString: String = "Arbitrary"+"."+"Choose"+"("+ count +", "+ elems +")"
  }
}
