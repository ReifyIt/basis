//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import basis.util._

/** Collections, containers, and macro operations.
  *
  * @contentDiagram hideNodes "basis.collections.Family" "basis.collections.From" "basis.collections.State"
  */
package object collections extends basis.collections.sequential.Strict {
  implicit def MaybeContainer[A](maybe: Maybe[A]): Container[A] =
    if (maybe.canBind) maybe.bind :: immutable.Nil else immutable.Nil
}
