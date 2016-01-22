//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class ArrayMapSpec extends FlatSpec with SubmapBehaviors {
  override def suiteName = "ArrayMap specification"

  override type Coll[X, Y] = ArrayMap[X, Y]
  override val Coll = ArrayMap

  it should behave like GenericMap()
  it should behave like GenericSubmap()
}
