/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Sequential collection operations. */
package object sequential {
  /** Implicit conversions to add general operations to collections. */
  val general = new General
  
  /** Implicit conversions to add general and non-strictly-evaluated operations to collections. */
  val nonstrict = new NonStrict
  
  /** Implicit conversions to add general and strictly-evaluated operations to collections. */
  val strict = new Strict
  
  private[sequential] val begin = new basis.control.Begin
}
