/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.collections.traversable._
import basis.sequential

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ArraySetSpec
  extends FunSpec
    with ShouldMatchers
    with generic.SetFactoryBehaviors
    with traversable.SetBehaviors
    with sequential.general.SetOpsBehaviors
    with sequential.strict.SetOpsBehaviors {
  
  override def suiteName = "ArraySet specification"
  
  it should behave like GenericSetFactory(ArraySet)
  
  it should behave like TraversableCollection(ArraySet)
  it should behave like TraversableContainer(ArraySet)
  it should behave like TraversableSet(ArraySet)
  
  it should behave like SequentialGeneralCollectionOps(ArraySet)
  it should behave like SequentialGeneralContainerOps(ArraySet)
  it should behave like SequentialGeneralSetOps(ArraySet)
  
  it should behave like SequentialStrictCollectionOps(ArraySet)
  it should behave like SequentialStrictContainerOps(ArraySet)
  it should behave like SequentialStrictSetOps(ArraySet)
}
