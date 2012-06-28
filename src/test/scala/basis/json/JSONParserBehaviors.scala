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

trait JSONParserBehaviors { this: FunSpec =>
  import ShouldMatchers._
  
  def ParsesComments(parse: String => JSValue) {
    it("should parse empty line comments") {
      parse("true //")
    }
    
    it("should parse empty block comments") {
      parse("true /**/")
    }
    
    it("should parse block comments containing slashes and asterisks") {
      parse("true /** /** /* / * // ** **/")
    }
    
    it("should parse preceding line comments") {
      parse("""// comment
      true""")
    }
    
    it("should parse succeeding line comments") {
      parse("true // comment")
    }
    
    it("should parse preceding block comments") {
      parse("/* comment */ true")
    }
    
    it("should parse succeeding block comments") {
      parse("true /* comment */")
    }
    
    it("should parse line comments in objects") {
      parse(""" { // here
        "true"    // here
        :         // here
        true      // here
        ,         // here
        "false"   // here
        :         // here
        false     // and here
      } """)
    }
    
    it("should parse line comments in arrays") {
      parse(""" [ // here
      true        // here
      ,           // here
      false       // and here
      ] """)
    }
    
    it("should parse block comments in objects") {
      parse(""" {
        /* here */ "true"  /* here */ : /* here */ true  /* and here */ ,
        /* here */ "false" /* here */ : /* here */ false /* and here */
      } """)
    }
    
    it("should parse block comments in arrays") {
      parse("[ /* here */ true /* here */ , /* here */ false /* and here */ ]")
    }
  }
  
  def ParsesValidJSON(parse: String => JSValue) {
    it("should parse empty objects") {
      parse("{ }") should equal (JSObject.empty)
    }
    
    it("should parse empty arrays") {
      parse("[ ]") should equal (JSArray.empty)
    }
    
    it("should parse empty strings") {
      parse("\"\"") should equal (JSString.empty)
    }
    
    it("should parse positive integers") {
      parse("0")  should equal (JSInteger(0))
      parse("1")  should equal (JSInteger(1))
      parse("5")  should equal (JSInteger(5))
      parse("10") should equal (JSInteger(10))
      parse("11") should equal (JSInteger(11))
      parse("15") should equal (JSInteger(15))
    }
    
    it("should parse negative integers") {
      parse("-0")  should equal (JSInteger(-0))
      parse("-1")  should equal (JSInteger(-1))
      parse("-5")  should equal (JSInteger(-5))
      parse("-10") should equal (JSInteger(-10))
      parse("-11") should equal (JSInteger(-11))
      parse("-15") should equal (JSInteger(-15))
    }
    
    it("should parse positive decimals") {
      parse("0.0")   should equal (JSDecimal(0.0))
      parse("0.5")   should equal (JSDecimal(0.5))
      parse("1.0")   should equal (JSDecimal(1.0))
      parse("1.5")   should equal (JSDecimal(1.5))
      parse("10.0")  should equal (JSDecimal(10.0))
      parse("10.5")  should equal (JSDecimal(10.5))
      parse("10.00") should equal (JSDecimal(10.00))
      parse("10.50") should equal (JSDecimal(10.50))
    }
    
    it("should parse negative decimals") {
      parse("-0.0")   should equal (JSDecimal(-0.0))
      parse("-0.5")   should equal (JSDecimal(-0.5))
      parse("-1.0")   should equal (JSDecimal(-1.0))
      parse("-1.5")   should equal (JSDecimal(-1.5))
      parse("-10.0")  should equal (JSDecimal(-10.0))
      parse("-10.5")  should equal (JSDecimal(-10.5))
      parse("-10.00") should equal (JSDecimal(-10.00))
      parse("-10.50") should equal (JSDecimal(-10.50))
    }
    
    it("should parse positive decimals with exponents") {
      parse("4e2")    should equal (JSDecimal(4e2))
      parse("4E2")    should equal (JSDecimal(4E2))
      parse("4e+2")   should equal (JSDecimal(4e+2))
      parse("4E+2")   should equal (JSDecimal(4E+2))
      parse("4e-2")   should equal (JSDecimal(4e-2))
      parse("4E-2")   should equal (JSDecimal(4E-2))
      parse("4.0e2")  should equal (JSDecimal(4.0e2))
      parse("4.0E2")  should equal (JSDecimal(4.0E2))
      parse("4.0e+2") should equal (JSDecimal(4.0e+2))
      parse("4.0E+2") should equal (JSDecimal(4.0E+2))
      parse("4.0e-2") should equal (JSDecimal(4.0e-2))
      parse("4.0E-2") should equal (JSDecimal(4.0E-2))
    }
    
    it("should parse negative decimals with exponents") {
      parse("-4e2")    should equal (JSDecimal(-4e2))
      parse("-4E2")    should equal (JSDecimal(-4E2))
      parse("-4e+2")   should equal (JSDecimal(-4e+2))
      parse("-4E+2")   should equal (JSDecimal(-4E+2))
      parse("-4e-2")   should equal (JSDecimal(-4e-2))
      parse("-4E-2")   should equal (JSDecimal(-4E-2))
      parse("-4.0e2")  should equal (JSDecimal(-4.0e2))
      parse("-4.0E2")  should equal (JSDecimal(-4.0E2))
      parse("-4.0e+2") should equal (JSDecimal(-4.0e+2))
      parse("-4.0E+2") should equal (JSDecimal(-4.0E+2))
      parse("-4.0e-2") should equal (JSDecimal(-4.0e-2))
      parse("-4.0E-2") should equal (JSDecimal(-4.0E-2))
    }
    
    it("should parse \"true\"") {
      parse("true") should equal (JSTrue)
    }
    
    it("should parse \"false\"") {
      parse("false") should equal (JSFalse)
    }
    
    it("should parse \"null\"") {
      parse("null") should equal (JSNull)
    }
    
    it("should orderly parse non-empty objects") {
      parse(""" {
        "object"  : {},
        "array"   : [],
        "string"  : "",
        "integer" : 0,
        "decimal" : 0.0,
        "true"    : true,
        "false"   : false,
        "null"    : null
      } """) should equal {
        JSObject(
          "object"  -> JSObject.empty,
          "array"   -> JSArray.empty,
          "string"  -> JSString.empty,
          "integer" -> JSInteger(0),
          "decimal" -> JSDecimal(0.0),
          "true"    -> JSTrue,
          "false"   -> JSFalse,
          "null"    -> JSNull)
      }
    }
    
    it("should orderly parse non-empty arrays") {
      parse(""" [{}, [], "", 0, 0.0, true, false, null] """) should equal {
        JSArray(JSObject.empty, JSArray.empty, JSString.empty,
        JSInteger(0), JSDecimal(0.0), JSTrue, JSFalse, JSNull)
      }
    }
    
    it("should parse objects nested in arrays") {
      parse(""" [{ "true" : true }, { "false" : false }] """) should equal {
        JSArray(JSObject("true" -> JSTrue), JSObject("false" -> JSFalse))
      }
    }
    
    it("should parse arrays nested in objects") {
      parse(""" { "a" : [true], "b" : [false] } """) should equal {
        JSObject("a" -> JSArray(JSTrue), "b" -> JSArray(JSFalse))
      }
    }
    
    it("should parse non-empty strings") {
      parse("\"test\"") should equal (JSString("test"))
    }
    
    it("should unescape parsed strings") {
      parse("\"\\\"\"") should equal (JSString("\""))
      parse("\"\\\'\"") should equal (JSString("\'"))
      parse("\"\\\\\"") should equal (JSString("\\"))
      parse("\"\\/\"")  should equal (JSString("/"))
      parse("\"\\b\"")  should equal (JSString("\b"))
      parse("\"\\f\"")  should equal (JSString("\f"))
      parse("\"\\n\"")  should equal (JSString("\n"))
      parse("\"\\r\"")  should equal (JSString("\r"))
      parse("\"\\t\"")  should equal (JSString("\t"))
    }
  }
  
  def RejectsInvalidJSON(parse: String => JSValue) {
    it("should not parse sequential values") {
      evaluating(parse("true false")) should produce [JSONException]
    }
    
    it("should not parse empty input") {
      evaluating(parse("")) should produce [JSONException]
    }
    
    it("should not parse unclosed empty objects") {
      evaluating(parse("{")) should produce [JSONException]
    }
    
    it("should not parse unclosed non-empty objects") {
      evaluating(parse("{\"true\":true")) should produce [JSONException]
    }
    
    it("should not parse objects with trailing commas") {
      evaluating(parse("{\"true\":true,}")) should produce [JSONException]
    }
    
    it("should not parse unclosed empty arrays") {
      evaluating(parse("[")) should produce [JSONException]
    }
    
    it("should not parse unclosed non-empty arrays") {
      evaluating(parse("[true")) should produce [JSONException]
    }
    
    it("should not parse arrays with trailing commas") {
      evaluating(parse("[true,]")) should produce [JSONException]
    }
    
    it("should not parse unclosed empty strings") {
      evaluating(parse("\"")) should produce [JSONException]
    }
    
    it("should not parse numbers with a leading zeros") {
      withClue("00")  (evaluating(parse("00"))  should produce [JSONException])
      withClue("01")  (evaluating(parse("01"))  should produce [JSONException])
      withClue("-00") (evaluating(parse("-00")) should produce [JSONException])
      withClue("-01") (evaluating(parse("-01")) should produce [JSONException])
    }
    
    it("should not parse numbers with a trailing decimal point") {
      withClue("0.")  (evaluating(parse("0."))  should produce [JSONException])
      withClue("1.")  (evaluating(parse("1."))  should produce [JSONException])
      withClue("-0.") (evaluating(parse("-0.")) should produce [JSONException])
      withClue("-1.") (evaluating(parse("-1.")) should produce [JSONException])
    }
    
    it("should not parse numbers with an invalid exponent") {
      withClue("4.0e")  (evaluating(parse("4.0e"))  should produce [JSONException])
      withClue("4.0E")  (evaluating(parse("4.0E"))  should produce [JSONException])
      withClue("4.0e+") (evaluating(parse("4.0e+")) should produce [JSONException])
      withClue("4.0E+") (evaluating(parse("4.0E+")) should produce [JSONException])
      withClue("4.0e-") (evaluating(parse("4.0e-")) should produce [JSONException])
      withClue("4.0E-") (evaluating(parse("4.0E-")) should produce [JSONException])
    }
  }
}
