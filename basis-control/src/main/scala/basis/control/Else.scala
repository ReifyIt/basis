/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

import basis.runtime._
import basis.util.MurmurHash3._

/** A conditional binding; either an intentional [[Bind]] or an alternative [[Trap]].
  * [[ElseOps]] provides standard operations available to all `Else` values.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Conditional
  * 
  * @groupprio  Determining   1
  * @groupprio  Binding       2
  * @groupprio  Evaluating    3
  * @groupprio  Composing     4
  * @groupprio  Recovering    5
  * @groupname  Handling      Exception Handling
  * @groupprio  Handling      6
  */
sealed abstract class Else[@specialized(Int, Long, Float, Double, Boolean) +A, +B] private[control] {
  /** Returns `true` if this is a `Bind`, otherwise returns `false`.
    * @group Determining */
  def canBind: Boolean
  
  /** Returns `true` if this is a `Trap`, otherwise returns `false`.
    * @group Determining */
  def canTrap: Boolean
  
  /** Returns `true` if this is a `Trap` with a safe `trap` method
    * (one that won't throw an exception). Returns `false` if calling
    * `trap` ''will'' throw an exception.
    * @group Determining */
  def canSafelyTrap: Boolean
  
  /** Returns the value of this `Bind`, or throws an exception if `!canBind`.
    * If this is a `Trap[Throwable]`, throws the value of this `Trap`.
    * @group Binding */
  def bind: A
  
  /** Returns the value of this `Trap`, or throws an exception if `!canSafelyTrap`.
    * @group Binding */
  def trap: B
}

/** An intentional [[Else]] binding.
  * @group Conditional */
sealed abstract class Bind[+A] private[control] extends (A Else Nothing) {
  final override def canBind: Boolean = true
  
  final override def canTrap: Boolean = false
  
  final override def canSafelyTrap: Boolean = false
  
  final override def trap: Nothing = throw new UnsupportedOperationException("Can't trap Bind.")
}

private[control] final class BindInt(value: Int) extends Bind[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override val bind: Int = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindInt] && bind == other.asInstanceOf[BindInt].bind
  
  override def hashCode: Int =
    mash(mix(seed[Bind[_]], hash(bind)))
  
  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[control] final class BindLong(value: Long) extends Bind[Long] with Reified {
  protected override def T: TypeHint[Long] = TypeHint.Long
  
  override val bind: Long = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindLong] && bind == other.asInstanceOf[BindLong].bind
  
  override def hashCode: Int =
    mash(mix(seed[Bind[_]], hash(bind)))
  
  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[control] final class BindFloat(value: Float) extends Bind[Float] with Reified {
  protected override def T: TypeHint[Float] = TypeHint.Float
  
  override val bind: Float = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindFloat] && bind == other.asInstanceOf[BindFloat].bind
  
  override def hashCode: Int =
    mash(mix(seed[Bind[_]], hash(bind)))
  
  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[control] final class BindDouble(value: Double) extends Bind[Double] with Reified {
  protected override def T: TypeHint[Double] = TypeHint.Double
  
  override val bind: Double = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindDouble] && bind == other.asInstanceOf[BindDouble].bind
  
  override def hashCode: Int =
    mash(mix(seed[Bind[_]], hash(bind)))
  
  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

private[control] final class BindBoolean(value: Boolean) extends Bind[Boolean] with Reified {
  protected override def T: TypeHint[Boolean] = TypeHint.Boolean
  
  override val bind: Boolean = value
  
  override def toString: String = if (value) "True" else "False"
}

private[control] final class BindRef[+A](value: A) extends Bind[A] {
  override val bind: A = value
  
  override def equals(other: Any): Boolean =
    other.isInstanceOf[BindRef[_]] && bind.equals(other.asInstanceOf[BindRef[_]].bind)
  
  override def hashCode: Int =
    mash(mix(seed[Bind[_]], hash(bind)))
  
  override def toString: String =
    new java.lang.StringBuilder("Bind").append('(').append(bind).append(')').toString
}

/** A factory for [[Bind]] values.
  * 
  * @group  Conditional
  * 
  * @groupprio  Constructing  1
  */
object Bind {
  /** Binds a polymorphic value.
    * @group Constructing */
  def apply[A](value: A): Bind[A] = {
    if (value.isInstanceOf[Int]) new BindInt(value.asInstanceOf[Int]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Long]) new BindLong(value.asInstanceOf[Long]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Float]) new BindFloat(value.asInstanceOf[Float]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Double]) new BindDouble(value.asInstanceOf[Double]).asInstanceOf[Bind[A]]
    else if (value.isInstanceOf[Boolean]) (if (value.asInstanceOf[Boolean]) True else False).asInstanceOf[Bind[A]]
    else new BindRef(value)
  }
  
  /** Binds an `Int` value.
    * @group Constructing */
  def apply(value: Int): Bind[Int] = new BindInt(value)
  
  /** Binds a `Long` value.
    * @group Constructing */
  def apply(value: Long): Bind[Long] = new BindLong(value)
  
  /** Binds a `Float` value.
    * @group Constructing */
  def apply(value: Float): Bind[Float] = new BindFloat(value)
  
  /** Binds a `Double` value.
    * @group Constructing */
  def apply(value: Double): Bind[Double] = new BindDouble(value)
  
  /** Binds a `Boolean` value.
    * @group Constructing */
  def apply(value: Boolean): Bind[Boolean] = if (value) True else False
}

/** An alternative [[Else]] binding.
  * @group Conditional */
sealed abstract class Trap[+B] private[control] extends (Nothing Else B) {
  final override def canBind: Boolean = false
  
  final override def canTrap: Boolean = true
  
  override def bind: Nothing = throw new UnsupportedOperationException("Can't bind Trap.")
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
    mash(mix(seed[Trap[_]], hash(trap)))
  
  override def toString: String =
    new java.lang.StringBuilder("Trap").append('(').append(trap).append(')').toString
}

/** The unit [[Else]] binding, and a factory for [[Trap]] values.
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
  
  /** Returns a new `Trap`.
    * @group Constructing */
  def apply[B](value: B): Trap[B] = new TrapRef(value)
  
  /** Returns `true` for trap-safe exceptions, and `false` for exceptions
    * that should propagate.
    * @group Handling */
  def isNonFatal(e: Throwable): Boolean =
    e.isInstanceOf[StackOverflowError]   || !(
    e.isInstanceOf[InterruptedException] ||
    e.isInstanceOf[NotImplementedError]  ||
    e.isInstanceOf[VirtualMachineError]  ||
    e.isInstanceOf[LinkageError]         ||
    e.isInstanceOf[ThreadDeath])
  
  /** A fuse that traps non-fatal exceptions.
    * @group Handling */
  object NonFatal extends scala.runtime.AbstractFunction1[Throwable, Trap[Throwable]] {
    override def apply(e: Throwable): Trap[Throwable] =
      if (Trap.isNonFatal(e)) new TrapRef(e) else throw e
  }
}
