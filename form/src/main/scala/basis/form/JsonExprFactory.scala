//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis._
import basis.collections._
import basis.text._
import scala.reflect.macros._

private[form] final class JsonExprFactory[C <: blackbox.Context, V <: JsonVariant](val c: C)(v: C#Expr[V]) extends JsonFactory {
  import c.{ Expr, Tree, WeakTypeTag }
  import c.universe._
  import c.universe.internal._

  protected val variant: Expr[V] = v.asInstanceOf[Expr[V]]

  override type JsonValue     = Expr[V#AnyForm]
  override type JsonObject    = Expr[V#ObjectForm]
  override type JsonArray     = Expr[V#SeqForm]
  override type JsonString    = Expr[V#TextForm]
  override type JsonNumber    = Expr[V#NumberForm]
  override type JsonBoolean   = Expr[V#BoolForm]
  override type JsonNull      = Expr[V#NullForm]
  override type JsonUndefined = Expr[V#NoForm]

  implicit protected def AnyFormTag    = WeakTypeTag[V#AnyForm](VariantType("AnyForm"))
  implicit protected def ObjectFormTag = WeakTypeTag[V#ObjectForm](VariantType("ObjectForm"))
  implicit protected def SeqFormTag    = WeakTypeTag[V#SeqForm](VariantType("SeqForm"))
  implicit protected def TextFormTag   = WeakTypeTag[V#TextForm](VariantType("TextForm"))
  implicit protected def NumberFormTag = WeakTypeTag[V#NumberForm](VariantType("NumberForm"))
  implicit protected def BoolFormTag   = WeakTypeTag[V#BoolForm](VariantType("BoolForm"))
  implicit protected def NullFormTag   = WeakTypeTag[V#NullForm](VariantType("NullForm"))
  implicit protected def NoFormTag     = WeakTypeTag[V#NoForm](VariantType("NoForm"))

  override def JsonObjectValue(expr: Expr[V#ObjectForm]) = Expr[V#AnyForm](q"$variant.JsonObjectValue($expr)")
  override def JsonArrayValue(expr: Expr[V#SeqForm])     = Expr[V#AnyForm](q"$variant.JsonArrayValue($expr)")
  override def JsonStringValue(expr: Expr[V#TextForm])   = Expr[V#AnyForm](q"$variant.JsonStringValue($expr)")

  override def JsonObjectBuilder: Builder[(String, Expr[V#AnyForm])] with State[Expr[V#ObjectForm]] = new JsonObjectBuilder
  override def JsonArrayBuilder: Builder[Expr[V#AnyForm]] with State[Expr[V#SeqForm]]               = new JsonArrayBuilder
  override def JsonStringBuilder: StringBuilder with State[Expr[V#TextForm]]                        = new JsonStringBuilder

  override def JsonString(value: String)  = Expr[V#TextForm](if (value.length == 0) q"$variant.TextForm.empty" else q"$variant.JsonString($value)")
  override def JsonNumber(value: String)  = Expr[V#NumberForm](q"$variant.JsonNumber(${JsonNumberLiteral(value)})")
  override def JsonInteger(value: String) = Expr[V#NumberForm](q"$variant.JsonNumber(${JsonIntegerLiteral(value)})")
  override def JsonTrue                   = Expr[V#BoolForm](q"$variant.JsonTrue")
  override def JsonFalse                  = Expr[V#BoolForm](q"$variant.JsonFalse")
  override def JsonNull                   = Expr[V#NullForm](q"$variant.JsonNull")
  override def JsonUndefined              = Expr[V#NoForm](q"$variant.JsonUndefined")

  override def JsonNew(identifier: String, arguments: Expr[V#SeqForm]) = Expr[V#AnyForm](q"$variant.JsonNew($identifier, $arguments)")

  private def JsonNumberLiteral(value: String): Tree = {
    try Literal(Constant(java.lang.Double.parseDouble(value)))
    catch {
      case _: NumberFormatException =>
        Literal(Constant(value))
    }
  }

  private def JsonIntegerLiteral(value: String): Tree = {
    try Literal(Constant(java.lang.Integer.parseInt(value)))
    catch {
      case _: NumberFormatException =>
        try Literal(Constant(java.lang.Long.parseLong(value)))
        catch {
          case _: NumberFormatException =>
            Literal(Constant(value))
        }
    }
  }

  private final class JsonObjectBuilder extends Builder[(String, Expr[V#AnyForm])] with State[Expr[V#ObjectForm]] {
    private[this] val underlying                                    = mutable.ArrayBuffer.empty[(String, Expr[V#AnyForm])]
    override def clear(): Unit                                      = underlying.clear()
    override def expect(count: Int): this.type                      = { underlying.expect(count); this }
    override def append(fieldExpr: (String, Expr[V#AnyForm])): Unit = underlying.append(fieldExpr)

    override def state = Expr[V#ObjectForm] {
      if (underlying.isEmpty) q"$variant.ObjectForm.empty"
      else {
        val builder = underlying.foldLeft(q"$variant.JsonObjectBuilder.expect(${underlying.length})")((b, e) => q"$b += ((${e._1}, ${e._2}))")
        q"$builder.state"
      }
    }
  }

  private final class JsonArrayBuilder extends Builder[Expr[V#AnyForm]] with State[Expr[V#SeqForm]] {
    private[this] val underlying                     = mutable.ArrayBuffer.empty[Expr[V#AnyForm]]
    override def clear(): Unit                       = underlying.clear()
    override def expect(count: Int): this.type       = { underlying.expect(count); this }
    override def append(expr: Expr[V#AnyForm]): Unit = underlying.append(expr)

    override def state = Expr[V#SeqForm] {
      if (underlying.isEmpty) q"$variant.SeqForm.empty"
      else {
        val builder = underlying.foldLeft(q"$variant.JsonArrayBuilder.expect(${underlying.length})")((b, e) => q"$b += $e")
        q"$builder.state"
      }
    }
  }

  private final class JsonStringBuilder extends StringBuilder with State[Expr[V#TextForm]] {
    private[this] val underlying                = String.Builder
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def state: Expr[V#TextForm]        = JsonString(underlying.state)
  }

  protected def VariantType(name: String): Type =
    typeRef(variant.staticType, variant.staticType.member(TypeName(name)), Nil).dealias
}
