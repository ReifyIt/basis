//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.collections._
import basis.text._
import basis.util._
import scala.reflect.macros._

private[json] final class JsonExprFactory[C <: Context, V <: JsonVariant](val c: C)(v: C#Expr[V]) extends JsonFactory {
  import c.{ Expr, Tree, WeakTypeTag }

  val universe: c.universe.type = c.universe
  import universe._

  protected val variant: Expr[V] = v.asInstanceOf[Expr[V]]

  override type JsonValue     = Expr[V#AnyForm]
  override type JsonObject    = Expr[V#ObjectForm]
  override type JsonArray     = Expr[V#SeqForm]
  override type JsonString    = Expr[V#StringForm]
  override type JsonNumber    = Expr[V#NumberForm]
  override type JsonBoolean   = Expr[V#BooleanForm]
  override type JsonNull      = Expr[V#NullForm]
  override type JsonUndefined = Expr[V#UndefinedForm]

  implicit protected def AnyFormTag       = WeakTypeTag[V#AnyForm](depType(c)(variant, "AnyForm"))
  implicit protected def ObjectFormTag    = WeakTypeTag[V#ObjectForm](depType(c)(variant, "ObjectForm"))
  implicit protected def SeqFormTag       = WeakTypeTag[V#SeqForm](depType(c)(variant, "SeqForm"))
  implicit protected def StringFormTag    = WeakTypeTag[V#StringForm](depType(c)(variant, "StringForm"))
  implicit protected def NumberFormTag    = WeakTypeTag[V#NumberForm](depType(c)(variant, "NumberForm"))
  implicit protected def BooleanFormTag   = WeakTypeTag[V#BooleanForm](depType(c)(variant, "BooleanForm"))
  implicit protected def NullFormTag      = WeakTypeTag[V#NullForm](depType(c)(variant, "NullForm"))
  implicit protected def UndefinedFormTag = WeakTypeTag[V#UndefinedForm](depType(c)(variant, "UndefinedForm"))

  override def JsonObjectValue(expr: Expr[V#ObjectForm]) = Expr[V#AnyForm](q"$variant.JsonObjectValue($expr)")
  override def JsonArrayValue(expr: Expr[V#SeqForm])     = Expr[V#AnyForm](q"$variant.JsonArrayValue($expr)")
  override def JsonStringValue(expr: Expr[V#StringForm]) = Expr[V#AnyForm](q"$variant.JsonStringValue($expr)")

  override def JsonObjectBuilder(): Builder[(String, Expr[V#AnyForm])] with State[Expr[V#ObjectForm]] = new JsonObjectBuilder
  override def JsonArrayBuilder(): Builder[Expr[V#AnyForm]] with State[Expr[V#SeqForm]]               = new JsonArrayBuilder
  override def JsonStringBuilder(): StringBuilder with State[Expr[V#StringForm]]                      = new JsonStringBuilder

  override def JsonString(value: String)  = Expr[V#StringForm](if (value.length == 0) q"$variant.StringForm.empty" else q"$variant.JsonString($value)")
  override def JsonNumber(value: String)  = Expr[V#NumberForm](q"$variant.JsonNumber(${JsonNumberLiteral(value)})")
  override def JsonInteger(value: String) = Expr[V#NumberForm](q"$variant.JsonNumber(${JsonIntegerLiteral(value)})")
  override def JsonTrue                   = Expr[V#BooleanForm](q"$variant.JsonTrue")
  override def JsonFalse                  = Expr[V#BooleanForm](q"$variant.JsonFalse")
  override def JsonNull                   = Expr[V#NullForm](q"$variant.JsonNull")
  override def JsonUndefined              = Expr[V#UndefinedForm](q"$variant.JsonUndefined")

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

  private final class JsonStringBuilder extends StringBuilder with State[Expr[V#StringForm]] {
    private[this] val underlying                = UString.Builder()
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def state: Expr[V#StringForm]      = JsonString(underlying.state.toString)
  }
}
