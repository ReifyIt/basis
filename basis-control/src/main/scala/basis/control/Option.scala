/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** [[Some]] value, or [[None]]
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Determining   1
  * @groupprio  Evaluating    2
  * @groupprio  Composing     3
  * @groupprio  Classifying   4
  */
sealed abstract class Option[@specialized(Int, Long, Float, Double, Boolean) +A] protected {
  /** Returns `true` if this `Option` contains `Some` value.
    * @group Determining */
  def isDefined: Boolean
  
  /** Returns `Some` value, or throws an exception if `None`.
    * @group Evaluating */
  def get: A
  
  /** Returns `true` if this `Option` equals another `Option`.
    * @group Classifying */
  override def equals(other: Any): Boolean = {
    (asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
    ((isInstanceOf[Some[_]] && other.isInstanceOf[Some[_]]) &&
      asInstanceOf[Some[_]].get == other.asInstanceOf[Some[_]].get)
  }
  
  /** Returns a hash of this `Option`.
    * @group Classifying */
  override def hashCode: Int = {
    import basis.util.MurmurHash3.{mash, mix}
    if (isInstanceOf[Some[_]]) mash(mix(-295060811, asInstanceOf[Some[_]].get.##))
    else if (isInstanceOf[None.type]) -295209735
    else throw new MatchError(this)
  }
  
  /** Returns a string representation of this `Option`.
    * @group Classifying */
  override def toString: String = {
    if (isInstanceOf[Some[_]])
      new java.lang.StringBuilder("Some").append('(').append(asInstanceOf[Some[_]].get).append(')').toString
    else if (isInstanceOf[None.type]) "None"
    else throw new MatchError(this)
  }
}

/** A factory for [[Option]] values. */
object Option {
  /** Returns `Some` non-`null` value, or `None` for `null`. */
  def apply[A](value: A): Option[A] = if (value != null) Some(value) else None
}

/** An [[Option]] with a defined value. */
sealed trait Some[+A] extends Option[A]

private[control] final class SomeInt(override val get: Int) extends Some[Int] {
  override def isDefined: Boolean = true
}

private[control] final class SomeLong(override val get: Long) extends Some[Long] {
  override def isDefined: Boolean = true
}

private[control] final class SomeFloat(override val get: Float) extends Some[Float] {
  override def isDefined: Boolean = true
}

private[control] final class SomeDouble(override val get: Double) extends Some[Double] {
  override def isDefined: Boolean = true
}

private[control] final class SomeBoolean(override val get: Boolean) extends Some[Boolean] {
  override def isDefined: Boolean = true
}

private[control] final class SomeRef[+A](override val get: A) extends Some[A] {
  override def isDefined: Boolean = true
}

/** A factory for [[Some]] values. */
object Some {
  /** Returns `Some` polymorphic value. */
  def apply[A](value: A): Some[A] = {
    if (value.isInstanceOf[Int]) new SomeInt(value.asInstanceOf[Int]).asInstanceOf[Some[A]]
    else if (value.isInstanceOf[Long]) new SomeLong(value.asInstanceOf[Long]).asInstanceOf[Some[A]]
    else if (value.isInstanceOf[Float]) new SomeFloat(value.asInstanceOf[Float]).asInstanceOf[Some[A]]
    else if (value.isInstanceOf[Double]) new SomeDouble(value.asInstanceOf[Double]).asInstanceOf[Some[A]]
    else if (value.isInstanceOf[Boolean]) (if (value.asInstanceOf[Boolean]) True else False).asInstanceOf[Some[A]]
    else new SomeRef(value)
  }
  
  /** Returns `Some` `Int` value. */
  def apply(value: Int): Some[Int] = new SomeInt(value)
  
  /** Returns `Some` `Long` value. */
  def apply(value: Long): Some[Long] = new SomeLong(value)
  
  /** Returns `Some` `Float` value. */
  def apply(value: Float): Some[Float] = new SomeFloat(value)
  
  /** Returns `Some` `Double` value. */
  def apply(value: Double): Some[Double] = new SomeDouble(value)
  
  /** Returns `Some` `Boolean` value */
  def apply(value: Boolean): Some[Boolean] = if (value) True else False
  
  private[this] val True: Some[Boolean] = new SomeBoolean(true)
  private[this] val False: Some[Boolean] = new SomeBoolean(false)
  
  /** Extracts `Some` value. */
  def unapply[A](some: Some[A]): scala.Some[A] = scala.Some(some.get)
}

/** An undefined [[Option]]. */
object None extends Option[Nothing] {
  override def isDefined: Boolean = false
  override def get: Nothing = throw new NoSuchElementException("None")
}
