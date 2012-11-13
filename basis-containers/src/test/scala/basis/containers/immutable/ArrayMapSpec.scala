/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.collections.general._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ArrayMapSpec
  extends FunSpec
    with ShouldMatchers
    with MapFactoryBehaviors
    with MapBehaviors {
  
  override def suiteName = "ArrayMap specification"
  
  it should behave like GenericMapFactory(ArrayMap)
  
  it should behave like GenericMap(ArrayMap)
}
