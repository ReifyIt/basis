/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

package object sequential {
  /** Implicit conversions that add common operations to collections. */
  val common = new ProvideCommonOps
  
  /** Implicit conversions that add common and strict operations to collections. */
  val strict = new ProvideEagerOps
  
  /** Implicit conversions that add command and non-strict operations to collections. */
  val nonstrict = new ProvideLazyOps
}
