//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.text._
import scala.reflect._

trait DateForm { variant: Variant =>
  type DateForm <: BaseDate with AnyForm

  val DateForm: BaseDateFactory

  implicit def DateFormTag: ClassTag[DateForm]

  trait BaseDate extends Equals with BaseValue { this: DateForm =>
    override def isDateForm: Boolean = true

    override def asDateForm: DateForm = this

    def millis: Long

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseDate]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseDate => that.canEqual(this) && millis == that.millis
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      mash(mix(seed[DateForm], hash(millis)))
    }

    override def toString: String = {
      val s = UString.Builder()
      s.append("DateForm")
      s.append('(')
      s.append(java.lang.Long.toString(millis))
      s.append('L')
      s.append(')')
      s.state.toString()
    }
  }

  trait BaseDateFactory {
    def apply(millis: Long): DateForm

    override def toString: String = "DateForm"
  }
}
