//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import scala.reflect._

trait SeqForm { variant: Variant =>
  type SeqForm <: BaseSeq with AnyForm

  val SeqForm: BaseSeqFactory

  implicit def SeqFormTag: ClassTag[SeqForm]

  trait BaseSeq
    extends Equals
    with Immutable
    with Family[SeqForm]
    with IndexedSeq[AnyForm]
    with BaseValue { this: SeqForm =>

    override def isSeqForm: Boolean = true

    override def asSeqForm: SeqForm = this

    override def / (index: Int): AnyForm = {
      if (0 <= index && index < length) this(index)
      else UndefinedForm
    }

    protected override def stringPrefix: String = "SeqForm"
  }

  trait BaseSeqFactory extends special.SeqSource[SeqForm, AnyForm] {
    override def toString: String = "SeqForm"
  }
}
