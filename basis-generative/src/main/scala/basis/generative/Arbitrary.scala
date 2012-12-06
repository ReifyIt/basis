/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.generative

import scala.annotation.{implicitNotFound, tailrec, unspecialized}

@implicitNotFound("No implicit generator available for Arbitrary[${R}].")
trait Arbitrary[@specialized(Specializable.Primitives) +R] extends (() => R) {
  @unspecialized def map[T](f: R => T): Arbitrary[T] =
    new Arbitrary.Map(this)(f)
  
  @unspecialized def flatMap[T](f: R => Arbitrary[T]): Arbitrary[T] =
    new Arbitrary.FlatMap(this)(f)
  
  @unspecialized def filter(p: R => Boolean): Arbitrary[R] =
    new Arbitrary.Filter(this, 1024)(p)
  
  @unspecialized def withFilter(p: R => Boolean): Arbitrary[R] =
    new Arbitrary.Filter(this, 1024)(p)
  
  override def toString: String = "<arbitrary>"
}

object Arbitrary {
  @inline def apply[R](implicit R: Arbitrary[R]): R.type = R
  
  implicit object Byte extends Arbitrary[Byte] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized(gen().toByte)
    override def toString = "Byte"
  }
  
  object PositiveByte extends Arbitrary[Byte] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized((gen() & 0x7F).toByte)
    override def toString = "PositiveByte"
  }
  
  implicit object Short extends Arbitrary[Short] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized(gen().toShort)
    override def toString = "Short"
  }
  
  object PositiveShort extends Arbitrary[Short] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized((gen() & 0x7FFF).toShort)
    override def toString = "PositiveShort"
  }
  
  implicit object Int extends Arbitrary[Int] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized(gen())
    override def toString = "Int"
  }
  
  object PositiveInt extends Arbitrary[Int] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized(gen() & 0x7FFFFFFF)
    override def toString = "PositiveInt"
  }
  
  private final class IntRange(lower: Int, upper: Int) extends Arbitrary[Int] {
    if (lower > upper) throw new IllegalArgumentException("lower > upper")
    private[this] val size: Long = (upper - lower) + 1L
    override def apply(): Int = (lower + (Arbitrary.PositiveInt() % size)).toInt
  }
  
  implicit object Long extends Arbitrary[Long] {
    private[this] val gen = new MersenneTwister64
    override def apply() = synchronized(gen())
    override def toString = "Long"
  }
  
  object PositiveLong extends Arbitrary[Long] {
    private[this] val gen = new MersenneTwister64
    override def apply() = synchronized(gen() & 0x7FFFFFFFFFFFFFFFL)
    override def toString = "PositiveLong"
  }
  
  implicit object Float extends Arbitrary[Float] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized((gen() >>> 8) / (1 << 24).toFloat)
    override def toString = "Float"
  }
  
  implicit object Double extends Arbitrary[Double] {
    private[this] val gen = new MersenneTwister64
    override def apply() = synchronized((gen() >>> 11) / (1L << 53).toDouble)
    override def toString = "Double"
  }
  
  implicit object Boolean extends Arbitrary[Boolean] {
    private[this] val gen = new MersenneTwister32
    override def apply() = synchronized((gen() & 1) != 0)
    override def toString = "AnyBoolean"
  }
  
  implicit object Unit extends Arbitrary[Unit] {
    override def apply() = ()
    override def toString = "Unit"
  }
  
  private final class Constant[@specialized(Specializable.Primitives) +R](value: R) extends Arbitrary[R] {
    override def apply() = value
    override def toString = value.toString
  }
  
  implicit def Constant[@specialized(Specializable.Primitives) R](value: R): Arbitrary[R] = new Constant(value)
  
  private final class Tuple2[+T1, +T2]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    extends Arbitrary[(T1, T2)] {
    override def apply() = (T1(), T2())
    override def toString = s"($T1, $T2)"
  }
  
  implicit def Tuple2[T1, T2]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    : Arbitrary[(T1, T2)] = new Tuple2
  
  private final class Tuple3[+T1, +T2, +T3]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    extends Arbitrary[(T1, T2, T3)] {
    override def apply() = (T1(), T2(), T3())
    override def toString = s"($T1, $T2, $T3)"
  }
  
  implicit def Tuple3[T1, T2, T3]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    : Arbitrary[(T1, T2, T3)] = new Tuple3
  
  private final class Tuple4[+T1, +T2, +T3, +T4]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    extends Arbitrary[(T1, T2, T3, T4)] {
    override def apply() = (T1(), T2(), T3(), T4())
    override def toString = s"($T1, $T2, $T3, $T4)"
  }
  
  implicit def Tuple4[T1, T2, T3, T4]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    : Arbitrary[(T1, T2, T3, T4)] = new Tuple4
  
  private final class Function1[-T1, +R]
      (f: T1 => R)
      (implicit T1: Arbitrary[T1])
    extends Arbitrary[R] {
    override def apply() = f(T1())
    override def toString = "<arbitrary1>"
  }
  
  def apply[T1, R]
      (f: T1 => R)
      (implicit T1: Arbitrary[T1])
    : Arbitrary[R] = new Function1(f)
  
  private final class Function2[-T1, -T2, +R]
      (f: (T1, T2) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    extends Arbitrary[R] {
    override def apply() = f(T1(), T2())
    override def toString = "<arbitrary2>"
  }
  
  def apply[T1, T2, R]
      (f: (T1, T2) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    : Arbitrary[R] = new Function2(f)
  
  private final class Function3[-T1, -T2, -T3, +R]
      (f: (T1, T2, T3) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    extends Arbitrary[R] {
    override def apply() = f(T1(), T2(), T3())
    override def toString = "<arbitrary3>"
  }
  
  def apply[T1, T2, T3, R]
      (f: (T1, T2, T3) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    : Arbitrary[R] = new Function3(f)
  
  private final class Function4[-T1, -T2, -T3, -T4, +R]
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    extends Arbitrary[R] {
    override def apply() = f(T1(), T2(), T3(), T4())
    override def toString = "<arbitrary4>"
  }
  
  def apply[T1, T2, T3, T4, R]
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    : Arbitrary[R] = new Function4(f)
  
  private final class Map[-T, +R](T: Arbitrary[T])(f: T => R) extends Arbitrary[R] {
    override def apply() = f(T())
    override def toString = s"$T.map($f)"
  }
  
  private final class FlatMap[-T, +R](T: Arbitrary[T])(f: T => Arbitrary[R]) extends Arbitrary[R] {
    override def apply() = f(T())()
    override def toString = s"$T.flatMap($f)"
  }
  
  private final class Filter[+R](R: Arbitrary[R], TTL: Int)(p: R => Boolean) extends Arbitrary[R] {
    @tailrec private[this] def apply(ttl: Int): R = {
      if (ttl < 0) throw new ArbitraryException("improbable filter")
      val x = R()
      if (p(x)) x
      else apply(ttl - 1)
    }
    override def apply() = apply(TTL)
    override def toString = s"$R.filter($p)"
  }
}
