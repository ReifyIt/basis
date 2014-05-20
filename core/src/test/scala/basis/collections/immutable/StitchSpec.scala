//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class StitchSpec extends FlatSpec with SeqBehaviors {
  override def suiteName = "Stitch specification"

  override type Coll[X] = Stitch[X]
  override val Coll = Stitch

  it should behave like GenericCollection()
  it should behave like GenericContainer()
  it should behave like GenericSeq()
  it should behave like SpecializedSeq()

  it should behave like GenericCollectionBuilder()
}
