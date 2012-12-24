/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** General utility macros. */
package object util {
  /** Implicitly adds arrow (-> and →) associators to all values. */
  implicit def ArrowOps[A](left: A): ArrowOps[A] =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  /** Implicitly adds supplemental operations to `Int` values. */
  implicit def IntOps(value: Int): IntOps =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  /** Implicitly adds supplemental operations to `Long` values. */
  implicit def LongOps(value: Long): LongOps =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  /** Implicitly adds supplemental operations to `Float` values. */
  implicit def FloatOps(value: Float): FloatOps =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
  
  /** Implicitly adds supplemental operations to `Double` values. */
  implicit def DoubleOps(value: Double): DoubleOps =
    throw new UnsupportedOperationException("Can't instantiate macro interface at runtime.")
}
