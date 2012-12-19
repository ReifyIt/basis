/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.generators

import basis.collections._
import basis.containers._

import scala.annotation.{implicitNotFound, tailrec, unspecialized}
import scala.reflect.ClassTag

@implicitNotFound("No generator available for ${R}.")
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
  def apply[R](implicit R: Arbitrary[R]): R.type = R
  
  implicit def Byte: Arbitrary[Byte] =
    new MersenneTwister32().asByte
  
  def PositiveByte: Arbitrary[Byte] =
    new MersenneTwister32().asPositiveByte
  
  implicit def Short: Arbitrary[Short] =
    new MersenneTwister32().asShort
  
  def PositiveShort: Arbitrary[Short] =
    new MersenneTwister32().asPositiveShort
  
  implicit def Int: Arbitrary[Int] =
    new MersenneTwister32()
  
  def PositiveInt: Arbitrary[Int] =
    new MersenneTwister32().asPositiveInt
  
  implicit def Long: Arbitrary[Long] =
    new MersenneTwister64()
  
  def PositiveLong: Arbitrary[Long] =
    new MersenneTwister64().asPositiveLong
  
  implicit def Float: Arbitrary[Float] =
    new MersenneTwister32().asFloat
  
  implicit def Double: Arbitrary[Double] =
    new MersenneTwister64().asDouble
  
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
  
  private final class Pick[+R](elems: Index[R])(implicit rnd: Randomness) extends Arbitrary[R] {
    if (elems.isEmpty) throw new IllegalArgumentException("empty pick")
    
    private[this] val gen = rnd.below(elems.length)
    
    def this(elems: Enumerator[R])(implicit rnd: Randomness) =
      this((ArraySeq.Builder[R] ++= elems).state)(rnd)
    
    override def apply(): R = elems(gen())
    
    override def toString: String = "<pick>"
  }
  
  def pick[R](elems: Enumerator[R]): Arbitrary[R] = new Pick(elems)
  
  private final class Choose[+R]
      (count: Int, elems: ListBuffer[R])
      (implicit gen: Randomness)
    extends Arbitrary[List[R]] {
    
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (count > elems.length) throw new IllegalArgumentException("count > length")
    
    def this(count: Int, elems: Enumerator[R])(implicit gen: Randomness) =
      this(count, new ListBuffer[R] ++= elems)(gen)
    
    override def apply(): List[R] = {
      val xs = elems.copy
      while (xs.length > count) xs.remove(gen.nextBelow(xs.length))
      xs.toList
    }
    
    override def toString: String = "<choose>"
  }
  
  def choose[R](count: Int, elems: Enumerator[R]): Arbitrary[List[R]] = new Choose(count, elems)
  
  def between(lower: Byte, upper: Byte): Arbitrary[Byte] =
    new MersenneTwister32().between(lower, upper)
  
  def between(lower: Short, upper: Short): Arbitrary[Short] =
    new MersenneTwister32().between(lower, upper)
  
  def between(lower: Int, upper: Int): Arbitrary[Int] =
    new MersenneTwister32().between(lower, upper)
  
  def between(lower: Long, upper: Long): Arbitrary[Long] =
    new MersenneTwister64().between(lower, upper)
  
  def between(lower: Float, upper: Float): Arbitrary[Float] =
    new MersenneTwister32().between(lower, upper)
  
  def between(lower: Double, upper: Double): Arbitrary[Double] =
    new MersenneTwister64().between(lower, upper)
  
  def below(upper: Byte): Arbitrary[Byte] =
    new MersenneTwister32().below(upper)
  
  def below(upper: Short): Arbitrary[Short] =
    new MersenneTwister32().below(upper)
  
  def below(upper: Int): Arbitrary[Int] =
    new MersenneTwister32().below(upper)
  
  def below(upper: Long): Arbitrary[Long] =
    new MersenneTwister32().below(upper)
  
  def below(upper: Float): Arbitrary[Float] =
    new MersenneTwister32().below(upper)
  
  def below(upper: Double): Arbitrary[Double] =
    new MersenneTwister32().below(upper)
  
  private final class Digit(implicit rnd: Randomness) extends Arbitrary[Char] {
    private[this] val gen = rnd.between(48, 57)
    override def apply(): Char = gen().toChar
    override def toString: String = "Digit"
  }
  
  def Digit: Arbitrary[Char] = new Digit
  
  private final class Uppercase(implicit rnd: Randomness) extends Arbitrary[Char] {
    private[this] val gen = rnd.between(65, 90)
    override def apply(): Char = gen().toChar
    override def toString: String = "Uppercase"
  }
  
  def Uppercase: Arbitrary[Char] = new Uppercase
  
  private final class Lowercase(implicit rnd: Randomness) extends Arbitrary[Char] {
    private[this] val gen = rnd.between(97, 122)
    override def apply(): Char = gen().toChar
    override def toString: String = "Lowercase"
  }
  
  def Lowercase: Arbitrary[Char] = new Lowercase
  
  private final class Alpha(implicit rnd: Randomness) extends Arbitrary[Char] {
    private[this] val gen = rnd.below(10)
    private[this] val lowercase = new Lowercase()(rnd)
    private[this] val uppercase = new Uppercase()(rnd)
    override def apply(): Char = if (gen() != 0) lowercase() else uppercase()
    override def toString: String = "Alpha"
  }
  
  def Alpha: Arbitrary[Char] = new Alpha
  
  private final class AlphaDigit(implicit rnd: Randomness) extends Arbitrary[Char] {
    private[this] val gen = rnd.below(10)
    private[this] val alpha = new Alpha()(rnd)
    private[this] val digit = new Digit()(rnd)
    override def apply(): Char = if (gen() != 0) alpha() else digit()
    override def toString: String = "AlphaDigit"
  }
  
  implicit def AlphaDigit: Arbitrary[Char] = new AlphaDigit
  
  private final class StringGenerator
      (maxLength: Int)
      (implicit C: Arbitrary[Char])
    extends Arbitrary[String] {
    
    private[this] val genLength = below(maxLength)
    
    override def apply(): String = {
      var i = 0
      val n = genLength()
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
  
  def String(maxLength: Int)(implicit C: Arbitrary[Char]): Arbitrary[String] =
    new StringGenerator(maxLength)(C)
  
  implicit def String(implicit C: Arbitrary[Char]): Arbitrary[String] =
    new StringGenerator(64)(C)
  
  private final class CollectionGenerator[CC[_], A]
      (length: Arbitrary[Int])
      (implicit CC: BuilderFactory[CC],
                A: Arbitrary[A],
                ATag: ClassTag[A])
    extends Arbitrary[CC[A]] {
    
    override def apply(): CC[A] = {
      var i = 0
      val n = length()
      val b = CC.Builder(ATag).expect(n)
      while (i < n) {
        b.append(A())
        i += 1
      }
      b.state
    }
    
    override def toString: String = "Collection"+"("+ CC +", "+ A +")"
  }
  
  def Collection[CC[_], A]
      (length: Arbitrary[Int])
      (implicit CC: BuilderFactory[CC],
                A: Arbitrary[A],
                ATag: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Arbitrary[CC[A]] =
    new CollectionGenerator(length)(CC, A, ATag)
  
  implicit def Collection[CC[_], A]
      (implicit CC: BuilderFactory[CC],
                A: Arbitrary[A],
                ATag: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Arbitrary[CC[A]] =
    new CollectionGenerator(below(64))(CC, A, ATag)
  
  private final class MapGenerator[CC[_, _], A, T]
      (length: Arbitrary[Int])
      (implicit CC: MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T],
                ATag: ClassTag[A],
                TTag: ClassTag[T])
    extends Arbitrary[CC[A, T]] {
    
    override def apply(): CC[A, T] = {
      var i = 0
      val n = length()
      val b = CC.Builder(ATag, TTag).expect(n)
      while (i < n) {
        b.append((A(), T()))
        i += 1
      }
      b.state
    }
    
    override def toString: String = "Map"+"("+ CC +", "+ A +", "+ T +")"
  }
  
  def Map[CC[_, _], A, T]
      (length: Arbitrary[Int])
      (implicit CC: MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T],
                ATag: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]],
                TTag: ClassTag[T] = ClassTag.Any.asInstanceOf[ClassTag[T]])
    : Arbitrary[CC[A, T]] =
    new MapGenerator(length)(CC, A, T, ATag, TTag)
  
  implicit def Map[CC[_, _], A, T]
      (implicit CC: MapFactory[CC],
                A: Arbitrary[A],
                T: Arbitrary[T],
                ATag: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]],
                TTag: ClassTag[T] = ClassTag.Any.asInstanceOf[ClassTag[T]])
    : Arbitrary[CC[A, T]] =
    new MapGenerator(below(64))(CC, A, T, ATag, TTag)
}
