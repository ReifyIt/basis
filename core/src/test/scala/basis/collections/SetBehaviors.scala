//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SetBehaviors extends ContainerBehaviors { this: FlatSpec =>
  import CollectionEnablers._
  import Matchers._

  override type Coll[X] <: Set[X]
  override val Coll: generic.SetFactory[Coll]

  def GenericSet(): Unit = {
    it should "have a zero size set" in {
      (Coll.empty: Set[Any]) should have size 0
    }
  }
}
