//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class ListSpec extends FunSpec with SeqBehaviors {
  override def suiteName = "List specification"

  it should behave like GenericCollection(List)
  it should behave like GenericContainer(List)
  it should behave like GenericSeq(List)

  it should behave like GenericCollectionBuilder(List)
}
