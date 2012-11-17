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

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HashMapSpec
  extends FunSpec
    with ShouldMatchers
    with MapFactoryBehaviors
    with MapBehaviors {
  
  override def suiteName = "HashMap specification"
  
  it should behave like GenericMapFactory(HashMap)
  
  it should behave like GenericMap(HashMap)
}
