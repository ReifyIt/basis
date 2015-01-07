//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class HashTrieSetSpec extends FlatSpec with SubsetBehaviors {
  override def suiteName = "HashTrieSet specification"

  override type Coll[X] = HashTrieSet[X]
  override val Coll = HashTrieSet

  it should behave like GenericCollection()
  it should behave like GenericContainer()
  it should behave like GenericSet()
  it should behave like GenericSubset()

  it should behave like GenericCollectionBuilder()
}
