//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

package object sequential {
  /** Implicit conversions that add general operations to collections.
    * @group Operations */
  val general: General = new General

  /** Implicit conversions that add general and strictly evaluated operations to collections.
    * @group Operations */
  val strict: Strict = new Strict

  /** Implicit conversions that add general and non-strictly evaluated operations to collections.
    * @group Operations */
  val nonstrict: NonStrict = new NonStrict
}
