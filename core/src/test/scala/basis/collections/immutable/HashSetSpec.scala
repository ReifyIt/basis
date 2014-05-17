//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class HashSetSpec extends FunSpec with SetBehaviors {
  override def suiteName = "HashSet specification"

  it should behave like GenericCollection(HashSet)
  it should behave like GenericContainer(HashSet)
  it should behave like GenericSet(HashSet)

  it should behave like GenericCollectionBuilder(HashSet)
}
