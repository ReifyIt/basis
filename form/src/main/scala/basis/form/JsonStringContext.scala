//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import scala.reflect.macros._

class JsonStringContext[-V <: JsonVariant](variant: V, stringContext: StringContext) {
  def json(args: V#AnyForm*): V#AnyForm        = macro JsonStringContextMacros.json[V]
  def jsobject(args: V#AnyForm*): V#ObjectForm = macro JsonStringContextMacros.jsobject[V]
  def jsarray(args: V#AnyForm*): V#SeqForm     = macro JsonStringContextMacros.jsarray[V]
}

private[form] class JsonVariantMacros(val c: blackbox.Context { type PrefixType <: JsonVariant }) {
  import c.{ Expr, mirror, prefix, WeakTypeTag }
  import c.universe._

  def JsonStringContext[V <: JsonVariant](stringContext: Expr[StringContext]): Expr[JsonStringContext[V]] = {
    implicit val JsonStringContextV = JsonStringContextTag[V]
    Expr[JsonStringContext[V]](q"new $JsonStringContextV($prefix, $stringContext)")
  }

  implicit private def JsonStringContextTag[V <: JsonVariant]: WeakTypeTag[JsonStringContext[V]] =
    WeakTypeTag[JsonStringContext[V]](
      appliedType(
        mirror.staticClass("basis.form.JsonStringContext").toTypeConstructor,
        (if (prefix.actualType != null) prefix.actualType else prefix.staticType) :: Nil))
}

private[form] class JsonStringContextMacros(val c: blackbox.Context { type PrefixType <: JsonStringContext[_] }) {
  import c.{ abort, Expr, prefix }
  import c.universe._

  @inline private def interpolate[V <: JsonVariant : WeakTypeTag, T](args: Seq[Expr[V#AnyForm]])(parse: (JsonInterpolator, JsonExprFactory[c.type, V]) => Expr[T]): Expr[T] = {
    val Typed(Apply(_, variant :: stringContext :: Nil), _) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val strings = scala.collection.mutable.ListBuffer.empty[String]
    val literals = stringLiterals.iterator
    while (literals.hasNext) {
      val Literal(Constant(string: String)) = literals.next()
      strings += string
    }

    val factory = new JsonExprFactory[c.type, V](c)(Expr[V](variant))
    val parser = new JsonInterpolator(strings, args.iterator)

    try {
      parser.skipWhitespace()
      val expr = parse(parser, factory)
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

  def json[V <: JsonVariant : WeakTypeTag](args: Expr[V#AnyForm]*): Expr[V#AnyForm] =
    interpolate[V, V#AnyForm](args)((parser, factory) => parser.parseValue(factory))

  def jsobject[V <: JsonVariant : WeakTypeTag](args: Expr[V#AnyForm]*): Expr[V#ObjectForm] =
    interpolate[V, V#ObjectForm](args)((parser, factory) => parser.parseObject(factory)(factory.JsonObjectBuilder))

  def jsarray[V <: JsonVariant : WeakTypeTag](args: Expr[V#AnyForm]*): Expr[V#SeqForm] =
    interpolate[V, V#SeqForm](args)((parser, factory) => parser.parseArray(factory)(factory.JsonArrayBuilder))
}
