//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import scala.reflect._

trait SetForm { variant: Variant =>
  type SetForm <: BaseSet with AnyForm

  val SetForm: BaseSetFactory

  implicit def SetFormTag: ClassTag[SetForm]

  trait BaseSet
    extends Equals
    with Immutable
    with Family[SetForm]
    with Set[AnyForm]
    with BaseValue { this: SetForm =>

    override def isSetForm: Boolean = true

    override def asSetForm: SetForm = this

    protected override def stringPrefix: String = "SetForm"
  }

  trait BaseSetFactory extends special.SetSource[SetForm, AnyForm] {
    override def toString: String = "SetForm"
  }
}
