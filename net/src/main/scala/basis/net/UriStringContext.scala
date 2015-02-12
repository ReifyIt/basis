//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import basis.collections._
import scala.reflect.macros._

class UriStringContext(stringContext: StringContext) {
  def uri(args: UriPart*): Uri = macro UriStringContextMacros.uri
}

private[net] class UriMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe._

  def UriStringContext(stringContext: Expr[StringContext]): Expr[UriStringContext] = {
    implicit val UriStringContext =
      WeakTypeTag[UriStringContext](mirror.staticClass("basis.net.UriStringContext").toType)
    Expr[UriStringContext](q"new $UriStringContext($stringContext)")
  }
}

private[net] class UriStringContextMacros(val c: blackbox.Context { type PrefixType <: UriStringContext }) {
  import c.{ abort, Expr, prefix }
  import c.universe._

  def uri(args: Expr[UriPart]*): Expr[Uri] = {
    val Typed(Apply(_, stringContext :: Nil), _) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val literals = stringLiterals.iterator
    val values = args.iterator

    val factory = new UriExprFactory[c.type](c)
    var parser = factory.UriParser: Iteratee[Int, Expr[Uri]]

    var input = null: LiteralIterator[c.type]
    while (literals.hasNext && parser.isCont) {
      input = new LiteralIterator(c, literals.next())
      while (!input.isEmpty && parser.isCont)
        parser = parser.feed(input)
      if (values.hasNext && parser.isCont)
        parser = parser.asInstanceOf[factory.Parser[Expr[Uri]]].interpolate(values.next())
    }
    if (!literals.hasNext && parser.isCont)
      parser = parser.feed(Iterator.done)
    if ((input ne null) && !input.isEmpty)
      parser = factory.error(input, expected = "valid URI character", found = input.head)
    if (parser.isDone) parser.bind
    else parser.trap match {
      case ex: UriException => abort(input.pos, ex.getMessage)
      case error => abort(input.pos, error.toString)
    }
  }
}

private[net] final class LiteralIterator[C <: blackbox.Context](
    val c: C,
    _literal: C#Tree,
    private[this] var index: Int)
  extends Iterator[Int] {

  def this(c: C, _literal: C#Tree) = this(c, _literal, 0)

  import c.universe._

  private[this] val literal: Tree = _literal.asInstanceOf[Tree]

  private[this] val Literal(Constant(string: String)) = literal

  def pos: Position = literal.pos.withPoint(literal.pos.point + index)

  override def isEmpty: Boolean = index >= string.length

  override def head: Int = {
    if (index >= string.length) Iterator.empty.head
    string.codePointAt(index)
  }

  override def step(): Unit = {
    if (index >= string.length) Iterator.empty.step()
    index = string.offsetByCodePoints(index, 1)
  }

  override def dup: Iterator[Int] = new LiteralIterator[C](c, literal, index)
}
