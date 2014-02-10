//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import scala.reflect._

trait UndefinedForm { variant: Variant =>
  type UndefinedForm <: BaseUndefined with AnyForm

  def UndefinedForm: UndefinedForm

  implicit def UndefinedFormTag: ClassTag[UndefinedForm]

  trait BaseUndefined extends BaseValue { this: UndefinedForm =>
    override def isDefined: Boolean = false

    override def isUndefinedForm: Boolean = true

    override def asUndefinedForm: UndefinedForm = this

    override def toString: String = "UndefinedForm"
  }
}
