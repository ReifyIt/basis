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
import basis.sequential

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HashSetSpec
  extends FunSpec
    with ShouldMatchers
    with generic.SetFactoryBehaviors
    with traversable.SetBehaviors
    with sequential.general.SetOpsBehaviors
    with sequential.strict.SetOpsBehaviors {
  
  override def suiteName = "HashSet specification"
  
  it should behave like GenericSetFactory(HashSet)
  
  it should behave like TraversableCollection(HashSet)
  it should behave like TraversableContainer(HashSet)
  it should behave like TraversableSet(HashSet)
  
  it should behave like SequentialGeneralCollectionOps(HashSet)
  it should behave like SequentialGeneralContainerOps(HashSet)
  it should behave like SequentialGeneralSetOps(HashSet)
  
  it should behave like SequentialStrictCollectionOps(HashSet)
  it should behave like SequentialStrictContainerOps(HashSet)
  it should behave like SequentialStrictSetOps(HashSet)
}
