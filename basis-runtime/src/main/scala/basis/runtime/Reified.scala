/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

/** A unary parametric type with a hint about its type parameter.
  * `Reified` protects its type hint so as not to clutter implementation
  * classes' APIs. Pattern match against [[Reified$ Reified]] to access
  * an instance's type hint. */
trait Reified extends Any {
  /** Returns a hint about this instance's type parameter. */
  protected def T: TypeHint[_]
}

/** An extractor for unary reified types. */
object Reified {
  /** Returns `true` if an instance has a matching reified type parameter. */
  def apply[T](any: Any)(implicit T: TypeHint[T]): Boolean =
    any.isInstanceOf[Reified] && any.asInstanceOf[Reified].T == T
  
  /** Extracts the type hint from a unary reified type. */
  def unapply(any: Reified): Some[TypeHint[_]] = Some(any.T)
}
