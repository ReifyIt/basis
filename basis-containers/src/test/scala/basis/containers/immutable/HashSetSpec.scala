/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HashSetSpec extends FunSpec with ShouldMatchers with SetBehaviors {
  override def suiteName = "HashSet"
  
  it should behave like genericCollection(HashSet)
  it should behave like genericContainer(HashSet)
  it should behave like genericSet(HashSet)
}
