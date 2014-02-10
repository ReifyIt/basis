//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.text._
import scala.reflect._

trait StringForm { variant: Variant =>
  type StringForm <: BaseString with AnyForm

  val StringForm: BaseStringFactory

  implicit def StringFormTag: ClassTag[StringForm]

  trait BaseString extends Equals with Family[StringForm] with UTF with BaseValue { this: StringForm =>
    override def isStringForm: Boolean = true

    override def asStringForm: StringForm = this

    def toUString: UString = new UString(super[UTF].toString)

    override def toString: String = {
      val s = UString.Builder()
      s.append("StringForm")
      s.append('(')
      show(s)
      s.append(')')
      s.state.toString
    }
  }

  trait BaseStringFactory extends StringFactory[StringForm] {
    override def toString: String = "StringForm"
  }
}
