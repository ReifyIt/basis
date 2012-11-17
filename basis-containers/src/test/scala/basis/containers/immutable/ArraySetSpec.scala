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

class ArraySetSpec
  extends FunSpec
    with ShouldMatchers
    with SetFactoryBehaviors
    with SetBehaviors
    with GeneralSetBehaviors
    with StrictSetBehaviors {
  
  override def suiteName = "ArraySet specification"
  
  it should behave like GenericSetFactory(ArraySet)
  
  it should behave like GenericCollection(ArraySet)
  it should behave like GenericContainer(ArraySet)
  it should behave like GenericSet(ArraySet)
  
  it should behave like GenericGeneralCollection(ArraySet)
  it should behave like GenericGeneralContainer(ArraySet)
  it should behave like GenericGeneralSet(ArraySet)
  
  it should behave like GenericStrictCollection(ArraySet)
  it should behave like GenericStrictContainer(ArraySet)
  it should behave like GenericStrictSet(ArraySet)
}
