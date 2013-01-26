/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

import basis.runtime._
import basis.util.MurmurHash3._

/** A conditional binding; either a [[Free]] value or a [[Trap]] value.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Conditional
  * 
  * @groupprio  Determining   1
  * @groupprio  Evaluating    2
  * @groupprio  Composing     3
  * @groupprio  Recovering    4
  * @groupname  Handling      Exception Handling
  * @groupprio  Handling      5
  */
sealed abstract class Else[@specialized(Int, Long, Float, Double, Boolean) +A, +B] private[control] {
  /** Returns `true` if this is a `Free` value, otherwise returns `false`.
    * @group Determining */
  def canBind: Boolean
  
  /** Returns `true` if this is a `Trap` value, otherwise returns `false`.
    * @group Determining */
  def canTrap: Boolean
  
  /** Returns `true` if this is a `Trap` value with a safe `trap` method
    * (one that won't throw an exception). Returns `false` if calling
    * `trap` will throw an exception.
    * @group Determining */
  def canSafelyTrap: Boolean
  
  /** Returns this `Free` value, otherwise throws an exception if `!canBind`.
    * @group Evaluating */
  def bind: A
  
  /** Returns this `Trap` value, otherwise throws an exception if `!canSafelyTrap`.
    * @group Evaluating */
  def trap: B
}

/** A consequent [[Else]] binding.
  * @group Conditional */
sealed abstract class Free[+A] private[control] extends (A Else Nothing) {
  final override def canBind: Boolean = true
  
  final override def canTrap: Boolean = false
  
  final override def canSafelyTrap: Boolean = false
  
  final override def trap: Nothing = throw new UnsupportedOperationException("Can't trap a Free value.")
}

private[control] final class FreeInt(value: Int) extends Free[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override val bind: Int = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[FreeInt] && bind == other.asInstanceOf[FreeInt].bind
  
  override def hashCode: Int =
    mash(mix(-295445459, bind.##))
  
  override def toString: String =
    new java.lang.StringBuilder("Free").append('(').append(bind).append(')').toString
}

private[control] final class FreeLong(value: Long) extends Free[Long] with Reified {
  protected override def T: TypeHint[Long] = TypeHint.Long
  
  override val bind: Long = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[FreeLong] && bind == other.asInstanceOf[FreeLong].bind
  
  override def hashCode: Int =
    mash(mix(-295445459, bind.##))
  
  override def toString: String =
    new java.lang.StringBuilder("Free").append('(').append(bind).append(')').toString
}

private[control] final class FreeFloat(value: Float) extends Free[Float] with Reified {
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override val bind: Float = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[FreeFloat] && bind == other.asInstanceOf[FreeFloat].bind
  
  override def hashCode: Int =
    mash(mix(-295445459, bind.##))
  
  override def toString: String =
    new java.lang.StringBuilder("Free").append('(').append(bind).append(')').toString
}

private[control] final class FreeDouble(value: Double) extends Free[Double] with Reified {
  protected override def T: TypeHint[Double] = TypeHint.Double
  
  override val bind: Double = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[FreeDouble] && bind == other.asInstanceOf[FreeDouble].bind
  
  override def hashCode: Int =
    mash(mix(-295445459, bind.##))
  
  override def toString: String =
    new java.lang.StringBuilder("Free").append('(').append(bind).append(')').toString
}

private[control] final class FreeBoolean(value: Boolean) extends Free[Boolean] with Reified {
  protected override def T: TypeHint[Boolean] = TypeHint.Boolean
  
  override val bind: Boolean = value
  
  override def toString: String =
    new java.lang.StringBuilder("Free").append('(').append(bind).append(')').toString
}

private[control] final class FreeRef[+A](value: A) extends Free[A] {
  override val bind: A = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[FreeRef[_]] && bind.equals(other.asInstanceOf[FreeRef[_]].bind)
  
  override def hashCode: Int =
    mash(mix(-295445459, bind.hashCode))
  
  override def toString: String =
    new java.lang.StringBuilder("Free").append('(').append(bind).append(')').toString
}

/** A factory for [[Free]] bindings.
  * 
  * @group  Conditional
  * 
  * @groupprio  Constructing  1
  */
object Free {
  /** Returns a `Free` binding with a polymorphic value.
    * @group Constructing */
  def apply[A](value: A): Free[A] = {
    if (value.isInstanceOf[Int]) new FreeInt(value.asInstanceOf[Int]).asInstanceOf[Free[A]]
    else if (value.isInstanceOf[Long]) new FreeLong(value.asInstanceOf[Long]).asInstanceOf[Free[A]]
    else if (value.isInstanceOf[Float]) new FreeFloat(value.asInstanceOf[Float]).asInstanceOf[Free[A]]
    else if (value.isInstanceOf[Double]) new FreeDouble(value.asInstanceOf[Double]).asInstanceOf[Free[A]]
    else if (value.isInstanceOf[Boolean]) (if (value.asInstanceOf[Boolean]) True else False).asInstanceOf[Free[A]]
    else new FreeRef(value)
  }
  
  /** Returns a `Free` binding with an `Int` value.
    * @group Constructing */
  def apply(value: Int): Free[Int] = new FreeInt(value)
  
  /** Returns a `Free` binding with a `Long` value.
    * @group Constructing */
  def apply(value: Long): Free[Long] = new FreeLong(value)
  
  /** Returns a `Free` binding with a `Float` value.
    * @group Constructing */
  def apply(value: Float): Free[Float] = new FreeFloat(value)
  
  /** Returns a `Free` binding with a `Double` value.
    * @group Constructing */
  def apply(value: Double): Free[Double] = new FreeDouble(value)
  
  private[this] val True: Free[Boolean] = new FreeBoolean(true)
  private[this] val False: Free[Boolean] = new FreeBoolean(false)
  
  /** Returns a `Free` binding with a `Boolean` value.
    * @group Constructing */
  def apply(value: Boolean): Free[Boolean] = if (value) True else False
}

/** An alternative [[Else]] binding.
  * @group Conditional */
sealed abstract class Trap[+B] private[control] extends (Nothing Else B) {
  final override def canBind: Boolean = false
  
  final override def canTrap: Boolean = true
  
  override def bind: Nothing = throw new UnsupportedOperationException("Can't bind a Trap.")
}

private[control] final class TrapRef[+B](value: B) extends Trap[B] {
  override def canSafelyTrap: Boolean = true
  
  override def bind: Nothing = {
    if (value.isInstanceOf[Throwable]) throw value.asInstanceOf[Throwable]
    else super.bind
  }
  
  override val trap: B = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[TrapRef[_]] && trap.equals(other.asInstanceOf[TrapRef[_]].trap)
  
  override def hashCode: Int =
    mash(mix(-295028498, trap.hashCode))
  
  override def toString: String =
    new java.lang.StringBuilder("Trap").append('(').append(trap).append(')').toString
}

/** The unit [[Else]] binding, and a factory for [[Trap]] bindings.
  * 
  * @group  Conditional
  * 
  * @groupprio  Constructing  1
  * @groupprio  Determining   2
  * @groupprio  Evaluating    3
  * @groupprio  Composing     4
  * @groupprio  Recovering    5
  * @groupprio  Handling      6
  * @groupprio  Classifying   7
  */
object Trap extends Trap[Nothing] {
  override def canSafelyTrap: Boolean = false
  
  override def trap: Nothing = throw new NoSuchElementException("Trap")
  
  /** Returns a string representing the unit `Trap`.
    * @group Classifying */
  override def toString: String = "Trap"
  
  /** Returns the unit `Trap` with some `Else` type.
    * @group Constructing */
  def apply[T >: Nothing Else Nothing]: T = this
  
  /** Returns a new `Trap` value.
    * @group Constructing */
  def apply[B](value: B): Trap[B] = new TrapRef(value)
  
  /** A fuse that traps non-fatal exceptions.
    * @group Handling */
  object NonFatal extends scala.runtime.AbstractFunction1[Throwable, Trap[Throwable]] {
    override def apply(e: Throwable): Trap[Throwable] = {
      if (e.isInstanceOf[StackOverflowError]   || !(
          e.isInstanceOf[InterruptedException] ||
          e.isInstanceOf[NotImplementedError]  ||
          e.isInstanceOf[VirtualMachineError]  ||
          e.isInstanceOf[LinkageError]         ||
          e.isInstanceOf[ThreadDeath]))
        new TrapRef(e)
      else throw e
    }
  }
}
