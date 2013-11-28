//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import scala.reflect._

trait AnyForm { variant: Variant =>
  type AnyForm <: BaseValue

  val AnyForm: BaseValueFactory

  implicit def AnyFormTag: ClassTag[AnyForm]

  trait BaseValue { this: AnyForm =>
    def isDefined: Boolean = true

    def isObjectForm: Boolean = false

    def asObjectForm: ObjectForm = throw new MatchError("not an ObjectForm")

    def isSeqForm: Boolean = false

    def asSeqForm: SeqForm = throw new MatchError("not a SeqForm")

    def isSetForm: Boolean = false

    def asSetForm: SetForm = throw new MatchError("not a SetForm")

    def isBinaryForm: Boolean = false

    def asBinaryForm: BinaryForm = throw new MatchError("not a BinaryForm")

    def isStringForm: Boolean = false

    def asStringForm: StringForm = throw new MatchError("not a StringForm")

    def isNumberForm: Boolean = false

    def asNumberForm: NumberForm = throw new MatchError("not a NumberForm")

    def isDateForm: Boolean = false

    def asDateForm: DateForm = throw new MatchError("not a DateForm")

    def isBooleanForm: Boolean = false

    def asBooleanForm: BooleanForm = throw new MatchError("not a BooleanForm")

    def isNullForm: Boolean = false

    def asNullForm: NullForm = throw new MatchError("not a NullForm")

    def isUndefinedForm: Boolean = false

    def asUndefinedForm: UndefinedForm = throw new MatchError("not an UndefinedForm")

    def / (key: String): AnyForm = UndefinedForm

    def / (index: Int): AnyForm = UndefinedForm
  }

  trait BaseValueFactory {
    override def toString: String = "AnyForm"
  }
}
