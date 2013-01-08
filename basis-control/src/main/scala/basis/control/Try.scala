/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** A computed [[Success]] value, or an exceptional [[Failure]].
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Determining   1
  * @groupprio  Evaluating    2
  * @groupprio  Composing     3
  * @groupprio  Recovering    4
  * @groupprio  Classifying   5
  */
sealed abstract class Try[@specialized(Int, Long, Float, Double, Boolean) +A] protected {
  /** Returns `true` if this is a `Success`, otherwise `false`.
    * @group Determining */
  def isSuccess: Boolean
  
  /** Returns `true` if this is a `Failure`, otherwise `false`.
    * @group Determining */
  def isFailure: Boolean
  
  /** Returns the value of a `Success`, or throws the exception of a `Failure`.
    * @group Evaluating */
  def get: A
  
  /** Returns `true` if this `Try` equals another `Try`.
    * @group Classifying */
  override def equals(other: Any): Boolean = {
    (asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
    ((isInstanceOf[Success[_]] && other.isInstanceOf[Success[_]]) &&
      asInstanceOf[Success[_]].get == other.asInstanceOf[Success[_]].get) ||
    ((isInstanceOf[Failure] && other.isInstanceOf[Failure]) &&
      asInstanceOf[Failure].cause == other.asInstanceOf[Failure].cause)
  }
  
  /** Returns a hash of this `Try`.
    * @group Classifying */
  override def hashCode: Int = {
    import basis.util.MurmurHash3.{mash, mix}
    if (isInstanceOf[Success[_]]) mash(mix(1804015266, asInstanceOf[Success[_]].get.##))
    else if (isInstanceOf[Failure]) mash(mix(-1710356439, asInstanceOf[Failure].cause.##))
    else throw new MatchError(this)
  }
  
  /** Returns a string representing the result of this `Try`.
    * @group Classifying */
  override def toString: String = {
    if (isInstanceOf[Success[_]])
      new java.lang.StringBuilder("Success").append('(').append(asInstanceOf[Success[_]].get).append(')').toString
    else if (isInstanceOf[Failure])
      new java.lang.StringBuilder("Failure").append('(').append(asInstanceOf[Failure].cause).append(')').toString
    else throw new MatchError(this)
  }
}

/** A factory for [[Try]] results. */
object Try {
  /** Evaluates `op` returning a `Success` with its value, or a `Failure`
    * for a caught, non-fatal exception. */
  def apply[A](op: => A): Try[A] = macro TryMacros.apply[A]
}

/** A successful [[Try]] result. */
sealed trait Success[+A] extends Try[A]

private[control] final class IntSuccess(override val get: Int) extends Success[Int] {
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

private[control] final class LongSuccess(override val get: Long) extends Success[Long] {
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

private[control] final class FloatSuccess(override val get: Float) extends Success[Float] {
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

private[control] final class DoubleSuccess(override val get: Double) extends Success[Double] {
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

private[control] final class BooleanSuccess(override val get: Boolean) extends Success[Boolean] {
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

private[control] final class RefSuccess[+A](override val get: A) extends Success[A] {
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

/** A factory for [[Success]] results. */
object Success {
  /** Returns a `Success` with the given polymorphic result. */
  def apply[A](value: A): Success[A] = {
    if (value.isInstanceOf[Int]) new IntSuccess(value.asInstanceOf[Int]).asInstanceOf[Success[A]]
    else if (value.isInstanceOf[Long]) new LongSuccess(value.asInstanceOf[Long]).asInstanceOf[Success[A]]
    else if (value.isInstanceOf[Float]) new FloatSuccess(value.asInstanceOf[Float]).asInstanceOf[Success[A]]
    else if (value.isInstanceOf[Double]) new DoubleSuccess(value.asInstanceOf[Double]).asInstanceOf[Success[A]]
    else if (value.isInstanceOf[Boolean]) (if (value.asInstanceOf[Boolean]) True else False).asInstanceOf[Success[A]]
    else new RefSuccess(value)
  }
  
  /** Returns a `Success` with the given `Int` result. */
  def apply(value: Int): Success[Int] = new IntSuccess(value)
  
  /** Returns a `Success` with the given `Long` result. */
  def apply(value: Long): Success[Long] = new LongSuccess(value)
  
  /** Returns a `Success` with the given `Float` result. */
  def apply(value: Float): Success[Float] = new FloatSuccess(value)
  
  /** Returns a `Success` with the given `Double` result. */
  def apply(value: Double): Success[Double] = new DoubleSuccess(value)
  
  /** Returns a `Success` with the given `Boolean` result. */
  def apply(value: Boolean): Success[Boolean] = if (value) True else False
  
  private[this] val True: Success[Boolean] = new BooleanSuccess(true)
  private[this] val False: Success[Boolean] = new BooleanSuccess(false)
  
  /** Extracts the value from a `Success`. */
  def unapply[A](success: Success[A]): scala.Some[A] = scala.Some(success.get)
}

/** An exceptional [[Try]] result. */
final class Failure private (exception: Throwable) extends Try[Nothing] {
  override def isSuccess: Boolean = false
  override def isFailure: Boolean = true
  override def get: Nothing = throw exception
  
  /** Returns the exception that caused this `Failure`.
    * @group Evaluating */
  def cause: Throwable = exception
}

/** A factory for [[Failure]] results. */
object Failure {
  /** Returns a `Failure` with an arbitrary `cause`. */
  val undefined: Failure = new Failure(new NoSuchElementException)
  
  /** Returns a `Failure` caused by the givem exception. */
  def apply(cause: Throwable): Failure = new Failure(cause)
  
  /** Extracts the `cause` from a `Failure`. */
  def unapply(failure: Failure): scala.Some[Throwable] = scala.Some(failure.cause)
}
