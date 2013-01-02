/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.runtime

/** A ternary parametric type with hints about its type parameters.
  * `Reified3` protects its type hints so as not to clutter implementation
  * classes' APIs. Pattern match against [[Reified3$ Reified3]] to access
  * an instance's type hints.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
trait Reified3 extends Any {
  /** Returns a hint about this instance's first type parameter. */
  protected def T1: TypeHint[_]
  
  /** Returns a hint about this instance's second type parameter. */
  protected def T2: TypeHint[_]
  
  /** Returns a hint about this instance's third type parameter. */
  protected def T3: TypeHint[_]
}

/** An extractor for ternary reified types. */
object Reified3 {
  /** Returns `true` if an instance has matching reified type parameters. */
  def apply[T1, T2, T3](any: Any)(implicit T1: TypeHint[T1], T2: TypeHint[T2], T3: TypeHint[T3]): Boolean =
    any.isInstanceOf[Reified3] && {
      val its = any.asInstanceOf[Reified3]
      its.T1 == T1 && its.T2 == T2 && its.T3 == T3
    }
  
  /** Extracts the type hints from a ternary reified type. */
  def unapply(any: Reified3): Some[(TypeHint[_], TypeHint[_], TypeHint[_])] =
    Some((any.T1, any.T2, any.T3))
}
