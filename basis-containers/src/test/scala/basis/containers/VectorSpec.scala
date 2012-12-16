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

class VectorSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with IndexBehaviors
    with sequential.GeneralIndexOpsBehaviors
    with sequential.StrictIndexOpsBehaviors {
  
  override def suiteName = "Vector specification"
  
  it should behave like GenericSeqFactory(Vector)
  
  it should behave like TraversableCollection(Vector)
  it should behave like TraversableContainer(Vector)
  it should behave like TraversableSeq(Vector)
  it should behave like TraversableIndex(Vector)
  
  it should behave like SequentialGeneralCollectionOps(Vector)
  it should behave like SequentialGeneralContainerOps(Vector)
  it should behave like SequentialGeneralSeqOps(Vector)
  it should behave like SequentialGeneralIndexOps(Vector)
  
  it should behave like SequentialStrictCollectionOps(Vector)
  it should behave like SequentialStrictContainerOps(Vector)
  it should behave like SequentialStrictSeqOps(Vector)
  it should behave like SequentialStrictIndexOps(Vector)
}
