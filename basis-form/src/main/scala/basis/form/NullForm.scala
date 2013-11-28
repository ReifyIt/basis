//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import scala.reflect._

trait NullForm { variant: Variant =>
  type NullForm <: BaseNull with AnyForm

  def NullForm: NullForm

  implicit def NullFormTag: ClassTag[NullForm]

  trait BaseNull extends BaseValue { this: NullForm =>
    override def isNullForm: Boolean = true

    override def asNullForm: NullForm = this

    override def toString: String = "NullForm"
  }
}
