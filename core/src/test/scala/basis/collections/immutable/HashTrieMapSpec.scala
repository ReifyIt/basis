//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class HashTrieMapSpec extends FlatSpec with SubmapBehaviors {
  override def suiteName = "HashTrieMap specification"

  override type Coll[X, Y] = HashTrieMap[X, Y]
  override val Coll = HashTrieMap

  it should behave like GenericMap()
  it should behave like GenericSubmap()
}
