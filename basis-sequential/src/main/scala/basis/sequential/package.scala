/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

/** Sequential collections operations.
  * 
  * @groupname  Operations  Collections use cases
  * @groupprio  Operations  1
  * 
  * @groupname  General     General collection extensions
  * @groupprio  General     2
  * 
  * @groupname  Strict      Strict collection extensions
  * @groupprio  Strict      3
  * 
  * @groupname  NonStrict   Non-strict collection extensions
  * @groupprio  NonStrict   4
  */
package object sequential {
  /** Implicit conversions to add general operations to collections.
    * @group Operations */
  val general = new General
  
  /** Implicit conversions to add general and strictly-evaluated operations to collections.
    * @group Operations */
  val strict = new Strict
  
  /** Implicit conversions to add general and non-strictly-evaluated operations to collections.
    * @group Operations */
  val nonstrict = new NonStrict
  
  private[sequential] val begin = new basis.control.Begin
}
