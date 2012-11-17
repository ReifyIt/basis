/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.collections.traversable._
import basis.collections.sequential._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ListSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with LinearSeqBehaviors
    with GeneralLinearSeqBehaviors
    with StrictLinearSeqBehaviors {
  
  override def suiteName = "List specification"
  
  it should behave like GenericSeqFactory(List)
  
  it should behave like GenericCollection(List)
  it should behave like GenericContainer(List)
  it should behave like GenericSeq(List)
  it should behave like GenericLinearSeq(List)
  
  it should behave like GenericGeneralCollection(List)
  it should behave like GenericGeneralContainer(List)
  it should behave like GenericGeneralSeq(List)
  it should behave like GenericGeneralLinearSeq(List)
  
  it should behave like GenericStrictCollection(List)
  it should behave like GenericStrictContainer(List)
  it should behave like GenericStrictSeq(List)
  it should behave like GenericStrictLinearSeq(List)
}
