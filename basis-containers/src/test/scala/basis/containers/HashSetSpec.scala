/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.sequential

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HashSetSpec
  extends FunSpec
    with ShouldMatchers
    with SetFactoryBehaviors
    with SetBehaviors
    with sequential.GeneralSetOpsBehaviors
    with sequential.StrictSetOpsBehaviors {
  
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