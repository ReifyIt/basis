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

class ListSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with QueueBehaviors
    with sequential.GeneralQueueOpsBehaviors
    with sequential.StrictQueueOpsBehaviors {
  
  override def suiteName = "List specification"
  
  it should behave like GenericSeqFactory(List)
  
  it should behave like TraversableCollection(List)
  it should behave like TraversableContainer(List)
  it should behave like TraversableSeq(List)
  it should behave like TraversableQueue(List)
  
  it should behave like SequentialGeneralCollectionOps(List)
  it should behave like SequentialGeneralContainerOps(List)
  it should behave like SequentialGeneralSeqOps(List)
  it should behave like SequentialGeneralQueueOps(List)
  
  it should behave like SequentialStrictCollectionOps(List)
  it should behave like SequentialStrictContainerOps(List)
  it should behave like SequentialStrictSeqOps(List)
  it should behave like SequentialStrictQueueOps(List)
}
