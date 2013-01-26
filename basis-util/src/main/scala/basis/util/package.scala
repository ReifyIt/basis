/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** General utility macros. */
package object util {
  /** Implicitly adds infix arrow (-> and →) associators to all values. */
  implicit def ArrowOps[A](left: A): ArrowOps[A] = new ArrowOps(left)
  
  /** Implicitly adds extended operations to `Int` values. */
  implicit def IntOps(a: Int): IntOps = new IntOps(a)
  
  /** Implicitly adds extended operations to `Long` values. */
  implicit def LongOps(a: Long): LongOps = new LongOps(a)
  
  /** Implicitly adds extended operations to `Float` values. */
  implicit def FloatOps(x: Float): FloatOps = new FloatOps(x)
  
  /** Implicitly adds extended operations to `Double` values. */
  implicit def DoubleOps(x: Double): DoubleOps = new DoubleOps(x)
}
