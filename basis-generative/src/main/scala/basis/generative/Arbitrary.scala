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

object Arbitrary {
  @inline def apply[R](implicit R: Arbitrary[R]): R.type = R
  
  implicit def Byte: Arbitrary[Byte] =
    new MersenneTwister32().asByte
  
  def Byte(lower: Byte, upper: Byte): Arbitrary[Byte] =
    new MersenneTwister32().asByte(lower, upper)
  
  def PositiveByte: Arbitrary[Byte] =
    new MersenneTwister32().asPositiveByte
  
  implicit def Short: Arbitrary[Short] =
    new MersenneTwister32().asShort
  
  def Short(lower: Short, upper: Short): Arbitrary[Short] =
    new MersenneTwister32().asShort(lower, upper)
  
  def PositiveShort: Arbitrary[Short] =
    new MersenneTwister32().asPositiveShort
  
  implicit def Int: Arbitrary[Int] =
    new MersenneTwister32()
  
  def Int(lower: Int, upper: Int): Arbitrary[Int] =
    new MersenneTwister32().asInt(lower, upper)
  
  def PositiveInt: Arbitrary[Int] =
    new MersenneTwister32().asPositiveInt
  
  implicit def Long: Arbitrary[Long] =
    new MersenneTwister64()
  
  def Long(lower: Long, upper: Long): Arbitrary[Long] =
    new MersenneTwister64().asLong(lower, upper)
  
  def PositiveLong: Arbitrary[Long] =
    new MersenneTwister64().asPositiveLong
  
  implicit def Float: Arbitrary[Float] =
    new MersenneTwister32().asFloat
  
  def Float(lower: Float, upper: Float): Arbitrary[Float] =
    new MersenneTwister32().asFloat(lower, upper)
  
  implicit def Double: Arbitrary[Double] =
    new MersenneTwister64().asDouble
  
  def Double(lower: Double, upper: Double): Arbitrary[Double] =
    new MersenneTwister64().asDouble(lower, upper)
  
  implicit def Boolean: Arbitrary[Boolean] =
    new MersenneTwister32().asBoolean
  
  private object Void extends Arbitrary[Unit] {
    override def apply(): Unit = ()
    override def toString: String = "Unit"
  }
  
  implicit def Unit: Arbitrary[Unit] = Void
  
  private final class Constant[@specialized(Specializable.Primitives) +R](value: R) extends Arbitrary[R] {
    override def apply(): R = value
    override def toString: String = value.toString
  }
  
  def Constant[@specialized(Specializable.Primitives) R](value: R): Arbitrary[R] = new Constant(value)
  
  private final class Tuple2[+T1, +T2]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    extends Arbitrary[(T1, T2)] {
    override def apply() = (T1(), T2())
    override def toString: String = s"($T1, $T2)"
  }
  
  implicit def Tuple2[T1, T2]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    : Arbitrary[(T1, T2)] = new Tuple2
  
  private final class Tuple3[+T1, +T2, +T3]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    extends Arbitrary[(T1, T2, T3)] {
    override def apply() = (T1(), T2(), T3())
    override def toString: String = s"($T1, $T2, $T3)"
  }
  
  implicit def Tuple3[T1, T2, T3]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    : Arbitrary[(T1, T2, T3)] = new Tuple3
  
  private final class Tuple4[+T1, +T2, +T3, +T4]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    extends Arbitrary[(T1, T2, T3, T4)] {
    override def apply() = (T1(), T2(), T3(), T4())
    override def toString: String = s"($T1, $T2, $T3, $T4)"
  }
  
  implicit def Tuple4[T1, T2, T3, T4]
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    : Arbitrary[(T1, T2, T3, T4)] = new Tuple4
  
  private final class Function1[-T1, +R]
      (f: T1 => R)
      (implicit T1: Arbitrary[T1])
    extends Arbitrary[R] {
    override def apply(): R = f(T1())
    override def toString: String = "<arbitrary1>"
  }
  
  def apply[T1, R]
      (f: T1 => R)
      (implicit T1: Arbitrary[T1])
    : Arbitrary[R] = new Function1(f)
  
  private final class Function2[-T1, -T2, +R]
      (f: (T1, T2) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    extends Arbitrary[R] {
    override def apply(): R = f(T1(), T2())
    override def toString: String = "<arbitrary2>"
  }
  
  def apply[T1, T2, R]
      (f: (T1, T2) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2])
    : Arbitrary[R] = new Function2(f)
  
  private final class Function3[-T1, -T2, -T3, +R]
      (f: (T1, T2, T3) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    extends Arbitrary[R] {
    override def apply(): R = f(T1(), T2(), T3())
    override def toString: String = "<arbitrary3>"
  }
  
  def apply[T1, T2, T3, R]
      (f: (T1, T2, T3) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3])
    : Arbitrary[R] = new Function3(f)
  
  private final class Function4[-T1, -T2, -T3, -T4, +R]
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    extends Arbitrary[R] {
    override def apply(): R = f(T1(), T2(), T3(), T4())
    override def toString: String = "<arbitrary4>"
  }
  
  def apply[T1, T2, T3, T4, R]
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Arbitrary[T1], T2: Arbitrary[T2], T3: Arbitrary[T3], T4: Arbitrary[T4])
    : Arbitrary[R] = new Function4(f)
}
