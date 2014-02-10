//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import scala.reflect._

trait BooleanForm { variant: Variant =>
  type BooleanForm <: BaseBoolean with AnyForm

  val BooleanForm: BaseBooleanFactory

  def TrueForm: BooleanForm

  def FalseForm: BooleanForm

  implicit def BooleanFormTag: ClassTag[BooleanForm]

  trait BaseBoolean extends Equals with BaseValue { this: BooleanForm =>
    override def isBooleanForm: Boolean = true

    override def asBooleanForm: BooleanForm = this

    def toBoolean: Boolean

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseBoolean]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseBoolean => that.canEqual(this) && toBoolean == that.toBoolean
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      mash(mix(seed[BooleanForm], hash(toBoolean)))
    }

    override def toString: String = if (toBoolean) "TrueForm" else "FalseForm"
  }

  trait BaseBooleanFactory {
    def apply(value: Boolean): BooleanForm =
      if (value) TrueForm else FalseForm

    override def toString: String = "BooleanForm"
  }
}
