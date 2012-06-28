/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import basis.json.model._

class JSONReaderSpec extends FunSpec with ShouldMatchers with JSONParserBehaviors {
  override def suiteName = "JSONReader specification"
  
  describe("Strict JSONReader") {
    val parse = (s: String) => {
      val parser = new JSONReader(s)
      parser.skipWhitespace()
      val jsvalue = parser.parseJSONValue(JSON)
      parser.skipWhitespace()
      parser.parseEnd()
      jsvalue
    }
    it should behave like ParsesComments(parse)
    it should behave like ParsesValidJSON(parse)
    it should behave like RejectsInvalidJSON(parse)
  }
}
