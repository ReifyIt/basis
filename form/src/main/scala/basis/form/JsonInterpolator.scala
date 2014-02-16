//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

private[form] final class JsonInterpolator(parts: scala.Seq[String], args: scala.Iterator[Any]) extends JsonParser {
  private[this] var partIndex: Int = 0
  private[this] var part: String = parts(0)

  private[this] var lookaheadIndex: Int = 0
  private[this] var lookahead: Int =
    if (part.length > 0) part.codePointAt(0)
    else if (parts.length > 1) {
      partIndex = 1
      part = parts(1)
      lookaheadIndex = -1
      0x1A // substitution
    }
    else -1

  protected override def isEOF: Boolean = lookahead < 0

  protected override def head: Int = lookahead

  protected override def step(): Unit = {
    if (lookahead < 0) throw new JsonException("unexpected end of input", partIndex, lookaheadIndex)
    if (lookaheadIndex < 0) {
      if (part.length > 0) {
        lookaheadIndex = 0
        lookahead = part.codePointAt(0)
      }
      else if (partIndex + 1 < parts.length) {
        partIndex += 1
        part = parts(partIndex)
        lookaheadIndex = -1
        lookahead = 0x1A // substitution
      }
      else {
        lookaheadIndex = 0
        lookahead = -1
      }
    }
    else {
      lookaheadIndex = part.offsetByCodePoints(lookaheadIndex, 1)
      if (lookaheadIndex < part.length) lookahead = part.codePointAt(lookaheadIndex)
      else if (partIndex + 1 < parts.length) {
        partIndex += 1
        part = parts(partIndex)
        lookaheadIndex = -1
        lookahead = 0x1A // substitution
      }
      else lookahead = -1
    }
  }

  protected override def substitute(factory: JsonFactory): factory.JsonValue = {
    if (lookahead(0x1A)) step() else syntaxError(describeInput("expected substitution"))
    args.next().asInstanceOf[factory.JsonValue]
  }

  protected override def syntaxError(message: String): Nothing =
    throw new JsonException(message, partIndex, lookaheadIndex)
}
