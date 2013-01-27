/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._

import scala.annotation.switch

/** Extended `String` operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
final class StringOps(s: String) {
  /** Returns this `String` quoted and escaped. */
  def show: String = {
    val builder = StringBuilder
    show(builder)
    builder.state
  }
  
  /** Appends this `String`, quoted and escaped, to the given builder. */
  def show(builder: Builder[Any, Int]) {
    builder += '\"'
    var i = 0
    val n = s.length
    while (i < n) {
      (s.codePointAt(i): @switch) match {
        case '\b' => builder += '\\' += 'b'
        case '\t' => builder += '\\' += 't'
        case '\n' => builder += '\\' += 'n'
        case '\f' => builder += '\\' += 'f'
        case '\r' => builder += '\\' += 'r'
        case '\"' => builder += '\\' += '\"'
        case '\\' => builder += '\\' += '\\'
        case c    => builder += c
      }
      i = s.offsetByCodePoints(i, 1)
    }
    builder += '\"'
  }
}
