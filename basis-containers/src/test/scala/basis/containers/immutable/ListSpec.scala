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

class ListSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with LinearSeqBehaviors
    with CommonLinearSeqBehaviors {
  
  override def suiteName = "List specification"
  
  it should behave like GenericSeqFactory(List)
  
  it should behave like GenericCollection(List)
  it should behave like GenericContainer(List)
  it should behave like GenericSeq(List)
  it should behave like GenericLinearSeq(List)
  
  it should behave like GenericCommonCollection(List)
  it should behave like GenericCommonContainer(List)
  it should behave like GenericCommonSeq(List)
  it should behave like GenericCommonLinearSeq(List)
}
