//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class BatchSpec extends FunSpec with SeqBehaviors {
  override def suiteName = "Batch specification"

  it should behave like GenericCollection(Batch)
  it should behave like GenericContainer(Batch)
  it should behave like GenericSeq(Batch)
}
