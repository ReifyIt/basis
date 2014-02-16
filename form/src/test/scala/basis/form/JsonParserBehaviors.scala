//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.util._
import org.scalatest._

trait JsonParserBehaviors { this: FunSpec =>
  import Matchers._

  val variant: JsonVariant
  import variant._
  import AnyForm.{ parseJson => Json }

  def ParsesJsonComments() = describe("parsing JSON comments") {
    it("should parse empty line comments") {
      Json("true //")
      ()
    }

    it("should parse empty block comments") {
      Json("true /**/")
      ()
    }

    it("should parse block comments containing slashes and asterisks") {
      Json("true /** /** /* / * // ** **/")
      ()
    }

    it("should parse preceding line comments") {
      Json("""// comment
      true""")
      ()
    }

    it("should parse succeeding line comments") {
      Json("true // comment")
      ()
    }

    it("should parse preceding block comments") {
      Json("/* comment */ true")
      ()
    }

    it("should parse succeeding block comments") {
      Json("true /* comment */")
      ()
    }

    it("should parse line comments in objects") {
      Json(""" { // here
        "true"    // here
        :         // here
        true      // here
        ,         // here
        "false"   // here
        :         // here
        false     // and here
      } """)
      ()
    }

    it("should parse line comments in arrays") {
      Json(""" [ // here
      true        // here
      ,           // here
      false       // and here
      ] """)
      ()
    }

    it("should parse block comments in objects") {
      Json(""" {
        /* here */ "true"  /* here */ : /* here */ true  /* and here */ ,
        /* here */ "false" /* here */ : /* here */ false /* and here */
      } """)
      ()
    }

    it("should parse block comments in arrays") {
      Json("[ /* here */ true /* here */ , /* here */ false /* and here */ ]")
      ()
    }
  }

  def ParsesJsonLiterals() = describe("parsing JSON literals") {
    it("should parse empty objects") {
      Json("{ }") should equal (ObjectForm.empty)
    }

    it("should parse empty arrays") {
      Json("[ ]") should equal (SeqForm.empty)
    }

    it("should parse empty strings") {
      Json("\"\"") should equal (StringForm.empty)
    }

    it("should parse positive integers") {
      Json("0")  should equal (NumberForm(0))
      Json("1")  should equal (NumberForm(1))
      Json("5")  should equal (NumberForm(5))
      Json("10") should equal (NumberForm(10))
      Json("11") should equal (NumberForm(11))
      Json("15") should equal (NumberForm(15))
    }

    it("should parse negative integers") {
      Json("-1")  should equal (NumberForm(-1))
      Json("-5")  should equal (NumberForm(-5))
      Json("-10") should equal (NumberForm(-10))
      Json("-11") should equal (NumberForm(-11))
      Json("-15") should equal (NumberForm(-15))
    }

    it("should parse positive decimals") {
      Json("0.0")   should equal (NumberForm(0.0))
      Json("0.5")   should equal (NumberForm(0.5))
      Json("1.0")   should equal (NumberForm(1.0))
      Json("1.5")   should equal (NumberForm(1.5))
      Json("10.0")  should equal (NumberForm(10.0))
      Json("10.5")  should equal (NumberForm(10.5))
      Json("10.00") should equal (NumberForm("10.00"))
      Json("10.50") should equal (NumberForm("10.50"))
    }

    it("should parse negative decimals") {
      Json("-0.0")   should equal (NumberForm(-0.0))
      Json("-0.5")   should equal (NumberForm(-0.5))
      Json("-1.0")   should equal (NumberForm(-1.0))
      Json("-1.5")   should equal (NumberForm(-1.5))
      Json("-10.0")  should equal (NumberForm(-10.0))
      Json("-10.5")  should equal (NumberForm(-10.5))
      Json("-10.00") should equal (NumberForm("-10.00"))
      Json("-10.50") should equal (NumberForm("-10.50"))
    }

    it("should parse positive decimals with exponents") {
      Json("4e2")    should equal (NumberForm("4e2"))
      Json("4E2")    should equal (NumberForm("4E2"))
      Json("4e+2")   should equal (NumberForm("4e+2"))
      Json("4E+2")   should equal (NumberForm("4E+2"))
      Json("4e-2")   should equal (NumberForm("4e-2"))
      Json("4E-2")   should equal (NumberForm("4E-2"))
      Json("4.0e2")  should equal (NumberForm("4.0e2"))
      Json("4.0E2")  should equal (NumberForm("4.0E2"))
      Json("4.0e+2") should equal (NumberForm("4.0e+2"))
      Json("4.0E+2") should equal (NumberForm("4.0E+2"))
      Json("4.0e-2") should equal (NumberForm("4.0e-2"))
      Json("4.0E-2") should equal (NumberForm("4.0E-2"))
    }

    it("should parse negative decimals with exponents") {
      Json("-4e2")    should equal (NumberForm("-4e2"))
      Json("-4E2")    should equal (NumberForm("-4E2"))
      Json("-4e+2")   should equal (NumberForm("-4e+2"))
      Json("-4E+2")   should equal (NumberForm("-4E+2"))
      Json("-4e-2")   should equal (NumberForm("-4e-2"))
      Json("-4E-2")   should equal (NumberForm("-4E-2"))
      Json("-4.0e2")  should equal (NumberForm("-4.0e2"))
      Json("-4.0E2")  should equal (NumberForm("-4.0E2"))
      Json("-4.0e+2") should equal (NumberForm("-4.0e+2"))
      Json("-4.0E+2") should equal (NumberForm("-4.0E+2"))
      Json("-4.0e-2") should equal (NumberForm("-4.0e-2"))
      Json("-4.0E-2") should equal (NumberForm("-4.0E-2"))
    }

    it("should parse \"true\"") {
      Json("true") should equal (TrueForm)
    }

    it("should parse \"false\"") {
      Json("false") should equal (FalseForm)
    }

    it("should parse \"null\"") {
      Json("null") should equal (NullForm)
    }

    it("should parse \"undefined\"") {
      Json("undefined") should equal (UndefinedForm)
    }

    it("should orderly parse non-empty objects") {
      Json(""" {
        "object"    : {},
        "array"     : [],
        "string"    : "",
        "integer"   : 0,
        "decimal"   : 0.0,
        "true"      : true,
        "false"     : false,
        "null"      : null,
        "undefined" : undefined
      } """) should equal (
        ObjectForm(
          ("object",    ObjectForm.empty),
          ("array",     SeqForm.empty),
          ("string",    StringForm.empty),
          ("integer",   NumberForm(0)),
          ("decimal",   NumberForm(0.0)),
          ("true",      TrueForm),
          ("false",     FalseForm),
          ("null",      NullForm),
          ("undefined", UndefinedForm)
        )
      )
    }

    it("should orderly parse non-empty arrays") {
      Json(""" [{}, [], "", 0, 0.0, true, false, null, undefined] """) should equal (
        SeqForm(
          ObjectForm.empty,
          SeqForm.empty,
          StringForm.empty,
          NumberForm(0),
          NumberForm(0.0),
          TrueForm,
          FalseForm,
          NullForm,
          UndefinedForm
        )
      )
    }

    it("should parse objects nested in arrays") {
      Json(""" [{ "true" : true }, { "false" : false }] """) should equal (
        SeqForm(ObjectForm("true" -> TrueForm), ObjectForm("false" -> FalseForm))
      )
    }

    it("should parse arrays nested in objects") {
      Json(""" { "a" : [true], "b" : [false] } """) should equal (
        ObjectForm("a" -> SeqForm(TrueForm), "b" -> SeqForm(FalseForm))
      )
    }

    it("should parse non-empty strings") {
      Json("\"test\"") should equal (StringForm("test"))
    }

    it("should unescape parsed strings") {
      Json("\"\\\"\"") should equal (StringForm("\""))
      Json("\"\\\'\"") should equal (StringForm("\'"))
      Json("\"\\\\\"") should equal (StringForm("\\"))
      Json("\"\\/\"")  should equal (StringForm("/"))
      Json("\"\\b\"")  should equal (StringForm("\b"))
      Json("\"\\f\"")  should equal (StringForm("\f"))
      Json("\"\\n\"")  should equal (StringForm("\n"))
      Json("\"\\r\"")  should equal (StringForm("\r"))
      Json("\"\\t\"")  should equal (StringForm("\t"))
    }
  }

  def RejectsInvalidJson() = describe("parsing invalid JSON") {
    it("should not parse sequential values") {
      evaluating(Json("true false")) should produce [JsonException]
      ()
    }

    it("should not parse empty input") {
      evaluating(Json("")) should produce [JsonException]
      ()
    }

    it("should not parse unclosed empty objects") {
      evaluating(Json("{")) should produce [JsonException]
      ()
    }

    it("should not parse unclosed non-empty objects") {
      evaluating(Json("{\"true\":true")) should produce [JsonException]
      ()
    }

    it("should not parse objects with trailing commas") {
      evaluating(Json("{\"true\":true,}")) should produce [JsonException]
      ()
    }

    it("should not parse unclosed empty arrays") {
      evaluating(Json("[")) should produce [JsonException]
      ()
    }

    it("should not parse unclosed non-empty arrays") {
      evaluating(Json("[true")) should produce [JsonException]
      ()
    }

    it("should not parse arrays with trailing commas") {
      evaluating(Json("[true,]")) should produce [JsonException]
      ()
    }

    it("should not parse unclosed empty strings") {
      evaluating(Json("\"")) should produce [JsonException]
      ()
    }

    it("should not parse numbers with a leading zeros") {
      withClue("00")  (evaluating(Json("00"))  should produce [JsonException])
      withClue("01")  (evaluating(Json("01"))  should produce [JsonException])
      withClue("-00") (evaluating(Json("-00")) should produce [JsonException])
      withClue("-01") (evaluating(Json("-01")) should produce [JsonException])
      ()
    }

    it("should not parse numbers with a trailing decimal point") {
      withClue("0.")  (evaluating(Json("0."))  should produce [JsonException])
      withClue("1.")  (evaluating(Json("1."))  should produce [JsonException])
      withClue("-0.") (evaluating(Json("-0.")) should produce [JsonException])
      withClue("-1.") (evaluating(Json("-1.")) should produce [JsonException])
      ()
    }

    it("should not parse numbers with an invalid exponent") {
      withClue("4.0e")  (evaluating(Json("4.0e"))  should produce [JsonException])
      withClue("4.0E")  (evaluating(Json("4.0E"))  should produce [JsonException])
      withClue("4.0e+") (evaluating(Json("4.0e+")) should produce [JsonException])
      withClue("4.0E+") (evaluating(Json("4.0E+")) should produce [JsonException])
      withClue("4.0e-") (evaluating(Json("4.0e-")) should produce [JsonException])
      withClue("4.0E-") (evaluating(Json("4.0E-")) should produce [JsonException])
      ()
    }
  }
}
