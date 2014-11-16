//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis._
import basis.collections._
import basis.text._
import basis.util._
import scala.reflect._

trait DeltaVariant extends Variant { variant =>
  /** A difference between variant forms.
    * @template */
  type AnyDelta >: AnyForm <: DeltaValue

  /** A difference between object forms.
    * @template */
  type ObjectDelta <: DeltaObject with AnyDelta

  /** A difference between set forms.
    * @template */
  type SetDelta <: DeltaSet with AnyDelta

  override type AnyForm    <: FormValue
  override type ObjectForm <: FormObject with AnyForm
  override type SetForm    <: FormSet with AnyForm

  val AnyDelta: DeltaValueFactory
  val ObjectDelta: DeltaObjectFactory
  val SetDelta: DeltaSetFactory

  implicit def AnyDeltaTag: ClassTag[AnyDelta]
  implicit def ObjectDeltaTag: ClassTag[ObjectDelta]
  implicit def SetDeltaTag: ClassTag[SetDelta]


  trait DeltaValue { this: AnyDelta =>
    def isDefined: Boolean = true
    def isDelta: Boolean   = true

    def isObjectDelta: Boolean = false
    def isSetDelta: Boolean    = false
    def isForm: Boolean        = false

    def asObjectDelta: ObjectDelta = throw new MatchError("not an ObjectDelta")
    def asSetDelta: SetDelta       = throw new MatchError("not a SetDelta")
    def asForm: AnyForm            = throw new MatchError("not an AnyForm")

    def / (key: String): AnyDelta = NoForm

    def in(domain: DeltaVariant): domain.AnyDelta
  }

  trait DeltaValueFactory {
    override def toString: String = (String.Builder~variant.toString~'.'~"AnyDelta").state
  }


  trait DeltaObject extends Equals with Immutable with Family[ObjectDelta] with Map[String, AnyDelta] with DeltaValue { this: ObjectDelta =>
    override def isObjectDelta: Boolean     = true
    override def asObjectDelta: ObjectDelta = this
    override def / (key: String): AnyDelta  = get(key).bindOrElse(NoForm)
    override def in(domain: DeltaVariant): domain.ObjectDelta =
      if (variant eq domain) asInstanceOf[domain.ObjectDelta]
      else this.map(field => field._1 -> field._2.in(domain))(domain.ObjectDelta.Builder)
    protected override def stringPrefix: String = ObjectDelta.toString
  }

  trait DeltaObjectFactory extends special.MapSource[ObjectDelta, String, AnyDelta] {
    override def toString: String = (String.Builder~variant.toString~'.'~"ObjectDelta").state
  }


  trait DeltaSet extends Equals with DeltaValue { this: SetDelta =>
    override def isSetDelta: Boolean  = true
    override def asSetDelta: SetDelta = this

    def deletions: SetForm
    def additions: SetForm

    override def in(domain: DeltaVariant): domain.SetDelta =
      if (variant eq domain) asInstanceOf[domain.SetDelta]
      else domain.SetDelta(deletions = deletions in domain, additions = additions in domain)

    override def canEqual(other: Any): Boolean = other.isInstanceOf[DeltaSet]

    override def equals(other: Any): Boolean = other match {
      case that: DeltaSet => that.canEqual(this) && deletions.equals(that.deletions) && additions.equals(that.additions)
      case _ => false
    }

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      mash(mix(mix(seed[SetDelta], deletions.hashCode), additions.hashCode))
    }

    override def toString: String =
      (String.Builder~variant.toString~'.'~"SetDelta"~'('~
        "deletions"~" = "~>deletions~", "~
        "additions"~" = "~>additions~')').state
  }

  trait DeltaSetFactory {
    def empty: SetDelta = apply(deletions = SetForm.empty, additions = SetForm.empty)
    def apply(deletions: SetForm = SetForm.empty, additions: SetForm = SetForm.empty): SetDelta
    override def toString: String = (String.Builder~variant.toString~'.'~"SetDelta").state
  }


  trait FormValue extends DeltaValue with BaseValue { this: AnyForm =>
    override def isDefined: Boolean = true
    override def isDelta: Boolean   = false

    override def isForm: Boolean = true
    override def asForm: AnyForm = this

    override def / (key: String): AnyForm = NoForm

    override def in(domain: DeltaVariant): domain.AnyDelta =
      in(domain: Variant).asInstanceOf[domain.AnyDelta]

    def delta(that: AnyForm): AnyDelta = that
    def patch(that: AnyDelta): AnyForm = if (that.isForm) that.asForm else this
  }


  trait FormObject extends FormValue with BaseObject { this: ObjectForm =>
    override def / (key: String): AnyForm = get(key).bindOrElse(NoForm)

    override def delta(that: AnyForm): AnyDelta =
      if (that.isObjectForm) delta(that.asObjectForm) else super.delta(that)

    def delta(that: ObjectForm): ObjectDelta = {
      val delta = ObjectDelta.Builder
      val these = iterator
      while (!these.isEmpty) {
        val field = these.head
        val key = field._1
        val x = field._2
        val y = that / key
        if (!y.isDefined) delta.append(key -> NoForm) // field removed
        else if ((x ne y) && x != y) delta.append(key -> (x delta y)) // field changed
        these.step()
      }
      val those = that.iterator
      while (!those.isEmpty) {
        val field = those.head
        if (!contains(field._1)) delta.append(field) // field added
        those.step()
      }
      delta.state
    }

    override def patch(that: AnyDelta): AnyForm =
      if (that.isObjectDelta) patch(that.asObjectDelta) else super.patch(that)

    def patch(that: ObjectDelta): ObjectForm = {
      var form = this
      val those = that.iterator
      while (!those.isEmpty) {
        val field = those.head
        val key = field._1
        val y = field._2
        if (y.isDefined) form += (key, (this / key) patch y)
        else form -= key
        those.step()
      }
      form
    }
  }


  trait FormSet extends FormValue with BaseSet { this: SetForm =>
    override def delta(that: AnyForm): AnyDelta =
      if (that.isSetForm) delta(that.asSetForm) else super.delta(that)

    def delta(that: SetForm): SetDelta = {
      val deletions = SetFormBuilder
      val additions = SetFormBuilder
      val these = iterator
      while (!these.isEmpty) {
        val x = these.head
        if (!that.contains(x)) deletions.append(x)
        these.step()
      }
      val those = that.iterator
      while (!those.isEmpty) {
        val y = those.head
        if (!contains(y)) additions.append(y)
        those.step()
      }
      SetDelta(deletions.state, additions.state)
    }

    override def patch(that: AnyDelta): AnyForm =
      if (that.isSetDelta) patch(that.asSetDelta) else super.patch(that)

    def patch(that: SetDelta): SetForm = {
      var form = this
      val deletions = that.deletions.iterator
      while (!deletions.isEmpty) {
        form -= deletions.head
        deletions.step()
      }
      val additions = that.additions.iterator
      while (!additions.isEmpty) {
        form += additions.head
        additions.step()
      }
      form
    }
  }
}
