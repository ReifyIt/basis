//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class ListSpec extends FlatSpec with SeqBehaviors {
  override def suiteName = "List specification"

  override type Coll[X] = List[X]
  override val Coll = List

  it should behave like GenericCollection()
  it should behave like GenericContainer()
  it should behave like GenericSeq()
  it should behave like SpecializedSeq()

  it should behave like GenericCollectionBuilder()
}
