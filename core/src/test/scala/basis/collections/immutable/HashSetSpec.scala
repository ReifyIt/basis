//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class HashSetSpec extends FlatSpec with SubSetBehaviors {
  override def suiteName = "HashSet specification"

  override type Coll[X] = HashSet[X]
  override val Coll = HashSet

  it should behave like GenericCollection()
  it should behave like GenericContainer()
  it should behave like GenericSet()
  it should behave like GenericSubSet()

  it should behave like GenericCollectionBuilder()
}
