//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.util._

class JsonStringContext[-V <: JsonVariant](variant: V, stringContext: StringContext) {
  def json(args: V#AnyForm*): V#AnyForm        = macro JsonStringContext.json[V]
  def jsobject(args: V#AnyForm*): V#ObjectForm = macro JsonStringContext.jsobject[V]
  def jsarray(args: V#AnyForm*): V#SeqForm     = macro JsonStringContext.jsarray[V]
}

private[json] object JsonStringContext {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def JsonStringContext[V <: JsonVariant]
      (c: ContextWithPre[V])
      (stringContext: c.Expr[StringContext])
    : c.Expr[JsonStringContext[V]] = {
    import c.{ Expr, mirror, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val JsonStringContextVTag =
      WeakTypeTag[JsonStringContext[V]](
        appliedType(
          mirror.staticClass("basis.form.json.JsonStringContext").toType,
          prefix.actualType :: Nil))
    Expr[JsonStringContext[V]](
      Apply(
        Select(New(TypeTree(weakTypeOf[JsonStringContext[V]])), nme.CONSTRUCTOR),
        prefix.tree :: stringContext.tree :: Nil))
  }

  def json[V <: JsonVariant : c.WeakTypeTag]
      (c: Context)
      (args: c.Expr[V#AnyForm]*)
    : c.Expr[V#AnyForm] = {
    import c.{ abort, Expr, prefix }
    import c.universe._

    val Apply(_, variant :: stringContext :: Nil) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val parts = new scala.collection.mutable.ArrayBuffer[String]
    val iter = stringLiterals.iterator
    while (iter.hasNext) {
      val Literal(Constant(string: String)) = iter.next()
      parts += string
    }

    val factory = new JsonExprFactory[c.type, V](c)(Expr[V](variant))
    val parser = new JsonInterpolator(parts, args.iterator)

    try {
      parser.skipWhitespace()
      val expr = parser.parseValue(factory)
      parser.skipWhitespace()
      parser.parseEOF()
      expr
    }
    catch {
      case e: JsonException =>
        val partPos = stringLiterals(e.part).pos
        val errorPos = partPos.withPoint(partPos.point + e.offset)
        abort(errorPos, e.getMessage)
    }
  }

  def jsobject[V <: JsonVariant : c.WeakTypeTag]
      (c: Context)
      (args: c.Expr[V#AnyForm]*)
    : c.Expr[V#ObjectForm] = {
    import c.{ abort, Expr, prefix }
    import c.universe._

    val Apply(_, variant :: stringContext :: Nil) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val parts = new scala.collection.mutable.ArrayBuffer[String]
    val iter = stringLiterals.iterator
    while (iter.hasNext) {
      val Literal(Constant(string: String)) = iter.next()
      parts += string
    }

    val factory = new JsonExprFactory[c.type, V](c)(Expr[V](variant))
    val parser = new JsonInterpolator(parts, args.iterator)

    try {
      parser.skipWhitespace()
      val expr = parser.parseObject(factory)(factory.JsonObjectBuilder)
      parser.skipWhitespace()
      parser.parseEOF()
      expr
    }
    catch {
      case e: JsonException =>
        val partPos = stringLiterals(e.part).pos
        val errorPos = partPos.withPoint(partPos.point + e.offset)
        abort(errorPos, e.getMessage)
    }
  }

  def jsarray[V <: JsonVariant : c.WeakTypeTag]
      (c: Context)
      (args: c.Expr[V#AnyForm]*)
    : c.Expr[V#SeqForm] = {
    import c.{ abort, Expr, prefix }
    import c.universe._

    val Apply(_, variant :: stringContext :: Nil) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val parts = new scala.collection.mutable.ArrayBuffer[String]
    val iter = stringLiterals.iterator
    while (iter.hasNext) {
      val Literal(Constant(string: String)) = iter.next()
      parts += string
    }

    val factory = new JsonExprFactory[c.type, V](c)(Expr[V](variant))
    val parser = new JsonInterpolator(parts, args.iterator)

    try {
      parser.skipWhitespace()
      val expr = parser.parseArray(factory)(factory.JsonArrayBuilder)
      parser.skipWhitespace()
      parser.parseEOF()
      expr
    }
    catch {
      case e: JsonException =>
        val partPos = stringLiterals(e.part).pos
        val errorPos = partPos.withPoint(partPos.point + e.offset)
        abort(errorPos, e.getMessage)
    }
  }
}
