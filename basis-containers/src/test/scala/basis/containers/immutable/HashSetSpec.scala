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

class HashSetSpec
  extends FunSpec
    with ShouldMatchers
    with SetFactoryBehaviors
    with SetBehaviors
    with CommonSetBehaviors {
  
  override def suiteName = "HashSet specification"
  
  it should behave like GenericSetFactory(HashSet)
  
  it should behave like GenericCollection(HashSet)
  it should behave like GenericContainer(HashSet)
  it should behave like GenericSet(HashSet)
  
  it should behave like GenericCommonCollection(HashSet)
  it should behave like GenericCommonContainer(HashSet)
  it should behave like GenericCommonSet(HashSet)
}
