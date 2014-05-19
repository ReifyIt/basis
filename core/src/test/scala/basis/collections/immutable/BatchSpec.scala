//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class BatchSpec extends FlatSpec with SeqBehaviors {
  override def suiteName = "Batch specification"

  override type Coll[X] = Batch[X]
  override val Coll = Batch

  it should behave like GenericCollection()
  it should behave like GenericContainer()
  it should behave like GenericSeq()
  it should behave like SpecializedSeq()

  it should behave like GenericCollectionBuilder()
}
