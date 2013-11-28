//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.collections._
import basis.text._
import scala.collection.immutable.{ ::, Nil }
import scala.reflect.macros.Context

private[json] final class JsonExprFactory[C <: Context, V <: JsonVariant]
    (val context: C)
    (_variant: C#Expr[V])
  extends JsonFactory {

  import context.{ Expr, mirror, weakTypeOf, WeakTypeTag }
  import universe._

  val universe: context.universe.type = context.universe

  protected val variant: Expr[V] = _variant.asInstanceOf[Expr[V]]

  override type JsonValue = Expr[V#AnyForm]

  override type JsonObject = Expr[V#ObjectForm]

  override type JsonArray = Expr[V#SeqForm]

  override type JsonString = Expr[V#StringForm]

  override type JsonNumber = Expr[V#NumberForm]

  override type JsonBoolean = Expr[V#BooleanForm]

  override type JsonNull = Expr[V#NullForm]

  override type JsonUndefined = Expr[V#UndefinedForm]

  override def JsonObjectValue(expr: Expr[V#ObjectForm]): Expr[V#AnyForm] =
    Expr[V#AnyForm](Apply(Select(variant.tree, "JsonObjectValue": TermName), expr.tree :: Nil))

  override def JsonArrayValue(expr: Expr[V#SeqForm]): Expr[V#AnyForm] =
    Expr[V#AnyForm](Apply(Select(variant.tree, "JsonArrayValue": TermName), expr.tree :: Nil))

  override def JsonStringValue(expr: Expr[V#StringForm]): Expr[V#AnyForm] =
    Expr[V#AnyForm](Apply(Select(variant.tree, "JsonStringValue": TermName), expr.tree :: Nil))

  override def JsonObjectBuilder(): Builder[(String, Expr[V#AnyForm])] with State[Expr[V#ObjectForm]] = new JsonObjectBuilder

  override def JsonArrayBuilder(): Builder[Expr[V#AnyForm]] with State[Expr[V#SeqForm]] = new JsonArrayBuilder

  override def JsonString(value: String): Expr[V#StringForm] = {
    if (value.length == 0) Expr[V#StringForm](Select(Select(variant.tree, "StringForm": TermName), "empty": TermName))
    else Expr[V#StringForm](Apply(Select(variant.tree, "JsonString": TermName), Literal(Constant(value)) :: Nil))
  }

  override def JsonStringBuilder(): StringBuilder with State[Expr[V#StringForm]] = new JsonStringBuilder

  override def JsonNumber(value: String): Expr[V#NumberForm] = {
    val literalValue =
      try Literal(Constant(java.lang.Double.parseDouble(value)))
      catch {
        case _: NumberFormatException =>
          Literal(Constant(value))
      }
    Expr[V#NumberForm](Apply(Select(variant.tree, "JsonNumber": TermName), literalValue :: Nil))
  }

  override def JsonInteger(value: String): Expr[V#NumberForm] = {
    val literalValue =
      try Literal(Constant(java.lang.Integer.parseInt(value)))
      catch {
        case _: NumberFormatException =>
          try Literal(Constant(java.lang.Long.parseLong(value)))
          catch {
            case _: NumberFormatException =>
              Literal(Constant(value))
          }
      }
    Expr[V#NumberForm](Apply(Select(variant.tree, "JsonNumber": TermName), literalValue :: Nil))
  }

  override def JsonTrue: Expr[V#BooleanForm] =
    Expr[V#BooleanForm](Select(variant.tree, "JsonTrue": TermName))

  override def JsonFalse: Expr[V#BooleanForm] =
    Expr[V#BooleanForm](Select(variant.tree, "JsonFalse": TermName))

  override def JsonNull: Expr[V#NullForm] =
    Expr[V#NullForm](Select(variant.tree, "JsonNull": TermName))

  override def JsonUndefined: Expr[V#UndefinedForm] =
    Expr[V#UndefinedForm](Select(variant.tree, "JsonUndefined": TermName))

  override def JsonNew(identifier: String, arguments: Expr[V#SeqForm]): Expr[V#AnyForm] =
    Expr[V#AnyForm](Apply(Select(variant.tree, "JsonNew": TermName), Literal(Constant(identifier)) :: arguments.tree :: Nil))

  implicit protected def AnyFormTag: WeakTypeTag[V#AnyForm] = {
    val AnyFormSym = variant.actualType.member("AnyForm": TypeName)
    val AnyFormTpe = typeRef(variant.actualType, AnyFormSym, Nil).normalize
    WeakTypeTag[V#AnyForm](AnyFormTpe)
  }

  implicit protected def ObjectFormTag: WeakTypeTag[V#ObjectForm] = {
    val ObjectFormSym = variant.actualType.member("ObjectForm": TypeName)
    val ObjectFormTpe = typeRef(variant.actualType, ObjectFormSym, Nil).normalize
    WeakTypeTag[V#ObjectForm](ObjectFormTpe)
  }

  implicit protected def SeqFormTag: WeakTypeTag[V#SeqForm] = {
    val SeqFormSym = variant.actualType.member("SeqForm": TypeName)
    val SeqFormTpe = typeRef(variant.actualType, SeqFormSym, Nil).normalize
    WeakTypeTag[V#SeqForm](SeqFormTpe)
  }

  implicit protected def StringFormTag: WeakTypeTag[V#StringForm] = {
    val StringFormSym = variant.actualType.member("StringForm": TypeName)
    val StringFormTpe = typeRef(variant.actualType, StringFormSym, Nil).normalize
    WeakTypeTag[V#StringForm](StringFormTpe)
  }

  implicit protected def NumberFormTag: WeakTypeTag[V#NumberForm] = {
    val NumberFormSym = variant.actualType.member("NumberForm": TypeName)
    val NumberFormTpe = typeRef(variant.actualType, NumberFormSym, Nil).normalize
    WeakTypeTag[V#NumberForm](NumberFormTpe)
  }

  implicit protected def BooleanFormTag: WeakTypeTag[V#BooleanForm] = {
    val BooleanFormSym = variant.actualType.member("BooleanForm": TypeName)
    val BooleanFormTpe = typeRef(variant.actualType, BooleanFormSym, Nil).normalize
    WeakTypeTag[V#BooleanForm](BooleanFormTpe)
  }

  implicit protected def NullFormTag: WeakTypeTag[V#NullForm] = {
    val NullFormSym = variant.actualType.member("NullForm": TypeName)
    val NullFormTpe = typeRef(variant.actualType, NullFormSym, Nil).normalize
    WeakTypeTag[V#NullForm](NullFormTpe)
  }

  implicit protected def UndefinedFormTag: WeakTypeTag[V#UndefinedForm] = {
    val UndefinedFormSym = variant.actualType.member("UndefinedForm": TypeName)
    val UndefinedFormTpe = typeRef(variant.actualType, UndefinedFormSym, Nil).normalize
    WeakTypeTag[V#UndefinedForm](UndefinedFormTpe)
  }

  private final class JsonObjectBuilder extends Builder[(String, Expr[V#AnyForm])] with State[Expr[V#ObjectForm]] {
    private[this] val underlying = mutable.ArrayBuffer.empty[(String, Expr[V#AnyForm])]

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(fieldExpr: (String, Expr[V#AnyForm])): Unit = underlying.append(fieldExpr)

    override def state: Expr[V#ObjectForm] = {
      if (underlying.isEmpty)
        Expr[V#ObjectForm](Select(Select(variant.tree, "ObjectForm": TermName), "empty": TermName))
      else {
        val Tuple2Tpe =
          appliedType(
            mirror.staticClass("scala.Tuple2").toType,
            definitions.StringClass.toType :: weakTypeOf[V#AnyForm] :: Nil)
        val builder =
          Apply(
            Select(Apply(Select(variant.tree, "JsonObjectBuilder": TermName), Nil), "expect": TermName),
            Literal(Constant(underlying.length)) :: Nil)
        val buildTree =
          underlying.foldLeft(builder)((builder, fieldExpr) =>
            Apply(
              Select(builder, ("+=": TermName).encodedName),
              Apply(
                Select(New(TypeTree(Tuple2Tpe)), nme.CONSTRUCTOR),
                Literal(Constant(fieldExpr._1)) :: fieldExpr._2.tree :: Nil) :: Nil))
        Expr[V#ObjectForm](Select(buildTree, "state": TermName))
      }
    }

    override def clear(): Unit = underlying.clear()
  }

  private final class JsonArrayBuilder extends Builder[Expr[V#AnyForm]] with State[Expr[V#SeqForm]] {
    private[this] val underlying = mutable.ArrayBuffer.empty[Expr[V#AnyForm]]

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(expr: Expr[V#AnyForm]): Unit = underlying.append(expr)

    override def state: Expr[V#SeqForm] = {
      if (underlying.isEmpty)
        Expr[V#SeqForm](Select(Select(variant.tree, "SeqForm": TermName), "empty": TermName))
      else {
        val builder =
          Apply(
            Select(Apply(Select(variant.tree, "JsonArrayBuilder": TermName), Nil), "expect": TermName),
            Literal(Constant(underlying.length)) :: Nil)
        val buildTree =
          underlying.foldLeft(builder)((builder, expr) =>
            Apply(Select(builder, ("+=": TermName).encodedName), expr.tree :: Nil))
        Expr[V#SeqForm](Select(buildTree, "state": TermName))
      }
    }

    override def clear(): Unit = underlying.clear()
  }

  private final class JsonStringBuilder extends StringBuilder with State[Expr[V#StringForm]] {
    private[this] val underlying = UString.Builder()

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(c: Int): Unit = underlying.append(c)

    override def append(cs: CharSequence): Unit = underlying.append(cs)

    override def state: Expr[V#StringForm] = JsonString(underlying.state.toString)

    override def clear(): Unit = underlying.clear()
  }
}
