/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.collections.generic._
import basis.collections.sequential._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class VectorSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with IndexedSeqBehaviors
    with CommonIndexedSeqBehaviors
    with StrictIndexedSeqBehaviors {
  
  override def suiteName = "Vector specification"
  
  it should behave like GenericSeqFactory(Vector)
  
  it should behave like GenericCollection(Vector)
  it should behave like GenericContainer(Vector)
  it should behave like GenericSeq(Vector)
  it should behave like GenericIndexedSeq(Vector)
  
  it should behave like GenericCommonCollection(Vector)
  it should behave like GenericCommonContainer(Vector)
  it should behave like GenericCommonSeq(Vector)
  it should behave like GenericCommonIndexedSeq(Vector)
  
  it should behave like GenericStrictCollection(Vector)
  it should behave like GenericStrictContainer(Vector)
  it should behave like GenericStrictSeq(Vector)
  it should behave like GenericStrictIndexedSeq(Vector)
}
