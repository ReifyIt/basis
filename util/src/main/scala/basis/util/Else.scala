//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

sealed abstract class Else[@specialized(Int, Long, Float, Double, Boolean) +A, +B] private[util] {
  def isDefined: Boolean

  def canBind: Boolean

  def canTrap: Boolean

  def canSafelyTrap: Boolean

  def get: A

  def bind: A

  def trap: B
}

sealed abstract class Bind[+A] private[util] extends (A Else Nothing) {
  final override def isDefined: Boolean = true

  final override def canBind: Boolean = true

  final override def canTrap: Boolean = false

  final override def canSafelyTrap: Boolean = false

  final override def trap: Nothing = throw new NoSuchElementException("trap")
}

private[util] final class BindInt(value: Int) extends Bind[Int] {
  override val bind: Int = value

  override def get: Int = bind

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindInt] && bind == other.asInstanceOf[BindInt].bind

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(0x0efa6427, hash(bind)))
  }

  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[util] final class BindLong(value: Long) extends Bind[Long] {
  override val bind: Long = value

  override def get: Long = bind

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindLong] && bind == other.asInstanceOf[BindLong].bind

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(0x0efa6427, hash(bind)))
  }

  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[util] final class BindFloat(value: Float) extends Bind[Float] {
  override val bind: Float = value

  override def get: Float = bind

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindFloat] && bind == other.asInstanceOf[BindFloat].bind

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(0x0efa6427, hash(bind)))
  }

  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[util] final class BindDouble(value: Double) extends Bind[Double] {
  override val bind: Double = value

  override def get: Double = bind

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindDouble] && bind == other.asInstanceOf[BindDouble].bind

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(0x0efa6427, hash(bind)))
  }

  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[util] final class BindBoolean(value: Boolean) extends Bind[Boolean] {
  override val bind: Boolean = value

  override def get: Boolean = bind

  override def toString: String = if (value) "True" else "False"
}

private[util] final class BindRef[+A](value: A) extends Bind[A] {
  override val bind: A = value

  override def get: A = bind

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindRef[_]] && bind == other.asInstanceOf[BindRef[_]].bind

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(0x0efa6427, hash(bind)))
  }

  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
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
}

sealed abstract class Trap[+B] private[util] extends (Nothing Else B) {
  final override def isDefined: Boolean = false

  final override def canBind: Boolean = false

  final override def canTrap: Boolean = true

  final override def get: Nothing = throw new NoSuchElementException("bind")
}

private[util] final class TrapRef[+B](value: B) extends Trap[B] {
  override def canSafelyTrap: Boolean = true

  override def bind: Nothing = {
    if (trap.isInstanceOf[Throwable]) throw trap.asInstanceOf[Throwable]
    else throw new NoSuchElementException("bind")
  }

  override val trap: B = value

  override def equals(other: Any): Boolean =
    other.isInstanceOf[TrapRef[_]] && trap == other.asInstanceOf[TrapRef[_]].trap

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(0x9aac7649, hash(trap)))
  }

  override def toString: String =
    new java.lang.StringBuilder("Trap").append('(').append(trap).append(')').toString
}

object Trap extends Trap[Nothing] {
  import scala.runtime._

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
