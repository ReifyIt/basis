/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

/** A binary parametric type with hints about its type parameters.
  * `Reified2` protects its type hints so as not to clutter implementation
  * classes' APIs. Pattern match against [[Reified2$ Reified2]] to access
  * an instance's type hints. */
trait Reified2[T1, T2] extends Any {
  /** Returns a hint about this instance's first type parameter. */
  protected def T1: TypeHint[T1]
  
  /** Returns a hint about this instance's second type parameter. */
  protected def T2: TypeHint[T2]
}

/** An extractor for binary reified types. */
object Reified2 {
  /** Returns `true` if an instance has matching reified type parameters. */
  def apply[T1, T2](any: Any)(implicit T1: TypeHint[T1], T2: TypeHint[T2]): Boolean =
    any.isInstanceOf[Reified2[_, _]] && {
      val its = any.asInstanceOf[Reified2[_, _]]
      its.T1 == T1 && its.T2 == T2
    }
  
  /** Extracts the type hints from a binary reified type. */
  def unapply[T1, T2](any: Reified2[T1, T2]): Some[(TypeHint[T1], TypeHint[T2])] =
     Some((any.T1, any.T2))
}
