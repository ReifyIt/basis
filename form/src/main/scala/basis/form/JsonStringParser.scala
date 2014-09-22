//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

private[form] final class JsonStringParser(input: String) extends JsonParser {
  private[this] var line: Int = 1
  private[this] var column: Int = 1

  private[this] var index: Int = 0
  private[this] var lookahead: Int = if (input.length > 0) input.codePointAt(0) else -1

  def remaining: String = input.substring(index)

  override def isEOF: Boolean = lookahead < 0

  protected override def head: Int = lookahead

  protected override def step(): Unit = {
    val c = lookahead
    if (c < 0) throw new JsonException("unexpected end of input", 0, index)
    index = input.offsetByCodePoints(index, 1)
    lookahead = if (index < input.length) input.codePointAt(index) else -1
    if (c == '\n' || (c == '\r' && lookahead == '\n')) { line += 1; column += 1 } else column += 1
  }

  protected override def syntaxError(message: String): Nothing = {
    val s = new java.lang.StringBuilder(message)
    s.append(" at line ").append(line).append(", column ").append(column)
    throw new JsonException(s.toString, 0, index)
  }
}
