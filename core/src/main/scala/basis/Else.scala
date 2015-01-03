//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import basis.collections._
import basis.text._

sealed abstract class Else[@specialized(Int, Long, Float, Double, Boolean) +A, +B] private[basis]
  extends Equals with Immutable with Family[Else[_, _]] with Container[A] {

  override def isEmpty: Boolean

  def isDefined: Boolean

  def isResult: Boolean

  def canBind: Boolean

  def canTrap: Boolean

  def canSafelyTrap: Boolean

  def get: A

  def bind: A

  def trap: B

  final override def canEqual(other: Any): Boolean = other.isInstanceOf[Else[_, _]]
}

sealed abstract class Bind[+A] private[basis] extends (A Else Nothing) {
  final override def isEmpty: Boolean = false

  final override def isDefined: Boolean = true

  final override def isResult: Boolean = true

  final override def canBind: Boolean = true

  final override def canTrap: Boolean = false

  final override def canSafelyTrap: Boolean = false

  final override def trap: Nothing = throw new NoSuchElementException("trap")

  protected final override def stringPrefix: String = "Bind"

  override def toString: String = (String.Builder~"Bind"~'('~>bind~')').state
}

private[basis] final class BindInt(value: Int) extends Bind[Int] {
  override val bind: Int = value

  override def get: Int = bind

  override def traverse(f: Int => Unit): Unit = f(bind)

  override def iterator: Iterator[Int] = new MaybeIntIterator(this)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindInt] && bind == other.asInstanceOf[BindInt].bind

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    mash(mix(seed[Bind[_]], hash(bind)))
  }
}

private[basis] final class BindLong(value: Long) extends Bind[Long] {
  override val bind: Long = value

  override def get: Long = bind

  override def traverse(f: Long => Unit): Unit = f(bind)

  override def iterator: Iterator[Long] = new MaybeLongIterator(this)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindLong] && bind == other.asInstanceOf[BindLong].bind

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    mash(mix(seed[Bind[_]], hash(bind)))
  }
}

private[basis] final class BindFloat(value: Float) extends Bind[Float] {
  override val bind: Float = value

  override def get: Float = bind

  override def traverse(f: Float => Unit): Unit = f(bind)

  override def iterator: Iterator[Float] = new MaybeFloatIterator(this)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindFloat] && bind == other.asInstanceOf[BindFloat].bind

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    mash(mix(seed[Bind[_]], hash(bind)))
  }
}

private[basis] final class BindDouble(value: Double) extends Bind[Double] {
  override val bind: Double = value

  override def get: Double = bind

  override def traverse(f: Double => Unit): Unit = f(bind)

  override def iterator: Iterator[Double] = new MaybeDoubleIterator(this)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindDouble] && bind == other.asInstanceOf[BindDouble].bind

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    mash(mix(seed[Bind[_]], hash(bind)))
  }
}

private[basis] final class BindBoolean(value: Boolean) extends Bind[Boolean] {
  override val bind: Boolean = value

  override def get: Boolean = bind

  override def traverse(f: Boolean => Unit): Unit = f(bind)

  override def iterator: Iterator[Boolean] = new TruthIterator(this)

  override def toString: String = if (value) "True" else "False"
}

private[basis] final class BindRef[+A](value: A) extends Bind[A] {
  override val bind: A = value

  override def get: A = bind

  override def traverse(f: A => Unit): Unit = f(bind)

  override def iterator: Iterator[A] = new MaybeRefIterator(this)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindRef[_]] && bind == other.asInstanceOf[BindRef[_]].bind

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    mash(mix(seed[Bind[_]], hash(bind)))
  }
}

object Bind {
  def apply[A](value: A): Bind[A] = {
    if (value.isInstanceOf[Int]) new BindInt(value.asInstanceOf[Int]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Long]) new BindLong(value.asInstanceOf[Long]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Float]) new BindFloat(value.asInstanceOf[Float]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Double]) new BindDouble(value.asInstanceOf[Double]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Boolean]) (if (value.asInstanceOf[Boolean]) True else False).asInstanceOf[Bind[A]]
    else new BindRef(value)
  }

  def apply(value: Int): Bind[Int] = new BindInt(value)

  def apply(value: Long): Bind[Long] = new BindLong(value)

  def apply(value: Float): Bind[Float] = new BindFloat(value)

  def apply(value: Double): Bind[Double] = new BindDouble(value)

  def apply(value: Boolean): Bind[Boolean] = if (value) True else False

  def unapply[A](maybe: A Else Any): A Else Any = maybe

  override def toString: String = "Bind"
}

sealed abstract class Trap[+B] private[basis] extends (Nothing Else B) {
  final override def isDefined: Boolean = false

  final override def isEmpty: Boolean = true

  final override def canBind: Boolean = false

  final override def canTrap: Boolean = true

  final override def get: Nothing = throw new NoSuchElementException("bind")

  final override def traverse(f: Nothing => Unit): Unit = ()

  final override def iterator: Iterator[Nothing] = Iterator.empty

  protected final override def stringPrefix: String = "Trap"
}

private[basis] final class TrapRef[+B](value: B) extends Trap[B] {
  override def isResult: Boolean = true

  override def canSafelyTrap: Boolean = true

  override def bind: Nothing = {
    if (trap.isInstanceOf[Throwable]) throw trap.asInstanceOf[Throwable]
    else throw new NoSuchElementException("bind")
  }

  override val trap: B = value

  override def equals(other: Any): Boolean =
    other.isInstanceOf[TrapRef[_]] && trap == other.asInstanceOf[TrapRef[_]].trap

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    mash(mix(seed[Trap[_]], hash(trap)))
  }

  override def toString: String = (String.Builder~"Trap"~'('~>trap~')').state
}

object Trap extends Trap[Nothing] {
  import scala.runtime._

  override def isResult: Boolean = false

  override def canSafelyTrap: Boolean = false

  override def bind: Nothing = throw new NoSuchElementException("bind")

  override def trap: Nothing = throw new NoSuchElementException("trap")

  override def toString: String = "Trap"

  def apply[B](value: B): Trap[B] = new TrapRef(value)

  def isNonFatal(e: Throwable): Boolean =
    e.isInstanceOf[StackOverflowError] || !(
      e.isInstanceOf[InterruptedException] ||
      e.isInstanceOf[NotImplementedError] ||
      e.isInstanceOf[VirtualMachineError] ||
      e.isInstanceOf[LinkageError] ||
      e.isInstanceOf[ThreadDeath])

  object NonFatal extends AbstractFunction1[Throwable, Trap[Throwable]] {
    override def apply(e: Throwable): Trap[Throwable] =
      if (isNonFatal(e)) new TrapRef(e) else throw e

    override def toString: String = "NonFatal"
  }
}

private[basis] final class MaybeIntIterator(private[this] var self: Maybe[Int]) extends Iterator[Int] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Int = if (self.canBind) self.bind else Iterator.empty.head

  override def step(): Unit = self = Trap

  override def dup: Iterator[Int] = new MaybeIntIterator(self)
}

private[basis] final class MaybeLongIterator(private[this] var self: Maybe[Long]) extends Iterator[Long] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Long = if (self.canBind) self.bind else Iterator.empty.head

  override def step(): Unit = self = Trap

  override def dup: Iterator[Long] = new MaybeLongIterator(self)
}

private[basis] final class MaybeFloatIterator(private[this] var self: Maybe[Float]) extends Iterator[Float] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Float = if (self.canBind) self.bind else Iterator.empty.head

  override def step(): Unit = self = Trap

  override def dup: Iterator[Float] = new MaybeFloatIterator(self)
}

private[basis] final class MaybeDoubleIterator(private[this] var self: Maybe[Double]) extends Iterator[Double] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Double = if (self.canBind) self.bind else Iterator.empty.head

  override def step(): Unit = self = Trap

  override def dup: Iterator[Double] = new MaybeDoubleIterator(self)
}

private[basis] final class TruthIterator(private[this] var self: Truth) extends Iterator[Boolean] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Boolean = if (self.canBind) self.bind else Iterator.empty.head

  override def step(): Unit = self = Trap

  override def dup: Iterator[Boolean] = new TruthIterator(self)
}

private[basis] final class MaybeRefIterator[+A](private[this] var self: Maybe[A]) extends Iterator[A] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: A = if (self.canBind) self.bind else Iterator.empty.head

  override def step(): Unit = self = Trap

  override def dup: Iterator[A] = new MaybeRefIterator(self)
}
