/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.sequential

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ListBufferSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with SeqBehaviors
    with sequential.GeneralSeqOpsBehaviors
    with sequential.StrictSeqOpsBehaviors {
  
  override def suiteName = "ListBuffer specification"
  
  it should behave like GenericSeqFactory(ListBuffer)
  
  it should behave like TraversableCollection(ListBuffer)
  it should behave like TraversableContainer(ListBuffer)
  it should behave like TraversableSeq(ListBuffer)
  
  it should behave like SequentialGeneralCollectionOps(ListBuffer)
  it should behave like SequentialGeneralContainerOps(ListBuffer)
  it should behave like SequentialGeneralSeqOps(ListBuffer)
  
  it should behave like SequentialStrictCollectionOps(ListBuffer)
  it should behave like SequentialStrictContainerOps(ListBuffer)
  it should behave like SequentialStrictSeqOps(ListBuffer)
}
