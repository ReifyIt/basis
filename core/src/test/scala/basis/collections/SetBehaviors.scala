//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SetBehaviors extends ContainerBehaviors { this: FunSpec =>
  import CollectionEnablers._
  import Matchers._

  def GenericSet[CC[X] <: Set[X]](CC: generic.SetFactory[CC]) = describe(s"generic $CC sets") {
    it("should have a zero size set") {
      (CC.empty: Set[Any]) should have size 0
    }
  }
}
