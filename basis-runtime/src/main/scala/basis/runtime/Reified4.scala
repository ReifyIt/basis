/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

/** A quaternary parametric type with hints about its type parameters.
  * `Reified4` protects its type hints so as not to clutter implementation
  * classes' APIs. Pattern match against [[Reified4$ Reified4]] to access
  * an instance's type hints. */
trait Reified4 extends Any {
  /** Returns a hint about this instance's first type parameter. */
  protected def T1: TypeHint[_]
  
  /** Returns a hint about this instance's second type parameter. */
  protected def T2: TypeHint[_]
  
  /** Returns a hint about this instance's third type parameter. */
  protected def T3: TypeHint[_]
  
  /** Returns a hint about this instance's fourth type parameter. */
  protected def T4: TypeHint[_]
}

/** An extractor for quaternary reified types. */
object Reified4 {
  /** Returns `true` if an instance has matching reified type parameters. */
  def apply[T1, T2, T3, T4](any: Any)
    (implicit T1: TypeHint[T1], T2: TypeHint[T2],
              T3: TypeHint[T3], T4: TypeHint[T4])
    : Boolean = any.isInstanceOf[Reified4] && {
      val its = any.asInstanceOf[Reified4]
      its.T1 == T1 && its.T2 == T2 && its.T3 == T3 && its.T4 == T4
    }
  
  /** Extracts the type hints from a quaternary reified type. */
  def unapply(any: Reified4): Some[(TypeHint[_], TypeHint[_], TypeHint[_], TypeHint[_])] =
    Some((any.T1, any.T2, any.T3, any.T4))
}
