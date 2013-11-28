//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import scala.reflect._

trait ObjectForm { variant: Variant =>
  type ObjectForm <: BaseObject with AnyForm

  val ObjectForm: BaseObjectFactory

  implicit def ObjectFormTag: ClassTag[ObjectForm]

  trait BaseObject
    extends Equals
    with Immutable
    with Family[ObjectForm]
    with Map[String, AnyForm]
    with BaseValue { this: ObjectForm =>

    override def isObjectForm: Boolean = true

    override def asObjectForm: ObjectForm = this

    override def / (key: String): AnyForm = get(key).bindOrElse(UndefinedForm)

    protected override def stringPrefix: String = "ObjectForm"
  }

  trait BaseObjectFactory extends special.MapSource[ObjectForm, String, AnyForm] {
    override def toString: String = "ObjectForm"
  }
}
