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

class ArraySeqSpec
  extends FunSpec
    with ShouldMatchers
    with SeqFactoryBehaviors
    with IndexBehaviors
    with sequential.GeneralIndexOpsBehaviors
    with sequential.StrictIndexOpsBehaviors {
  
  override def suiteName = "ArraySeq specification"
  
  it should behave like GenericSeqFactory(ArraySeq)
  
  it should behave like TraversableCollection(ArraySeq)
  it should behave like TraversableContainer(ArraySeq)
  it should behave like TraversableSeq(ArraySeq)
  it should behave like TraversableIndex(ArraySeq)
  
  it should behave like SequentialGeneralCollectionOps(ArraySeq)
  it should behave like SequentialGeneralContainerOps(ArraySeq)
  it should behave like SequentialGeneralSeqOps(ArraySeq)
  it should behave like SequentialGeneralIndexOps(ArraySeq)
  
  it should behave like SequentialStrictCollectionOps(ArraySeq)
  it should behave like SequentialStrictContainerOps(ArraySeq)
  it should behave like SequentialStrictSeqOps(ArraySeq)
  it should behave like SequentialStrictIndexOps(ArraySeq)
}
