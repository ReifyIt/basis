//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.util._
import org.scalatest._

trait JsonParserBehaviors extends Matchers { this: FlatSpec =>
  def ParsesJsonComments(variant: JsonVariant): Unit = {
    import variant._
    import AnyForm.{ parseJson => Json }

    it should "parse empty line comments" in {
      Json("true //")
      ()
    }

    it should "parse empty block comments" in {
      Json("true /**/")
      ()
    }

    it should "parse block comments containing slashes and asterisks" in {
      Json("true /** /** /* / * // ** **/")
      ()
    }

    it should "parse preceding line comments" in {
      Json("""// comment
      true""")
      ()
    }

    it should "parse succeeding line comments" in {
      Json("true // comment")
      ()
    }

    it should "parse preceding block comments" in {
      Json("/* comment */ true")
      ()
    }

    it should "parse succeeding block comments" in {
      Json("true /* comment */")
      ()
    }

    it should "parse line comments in objects" in {
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

    it should "parse line comments in arrays" in {
      Json(""" [ // here
      true        // here
      ,           // here
      false       // and here
      ] """)
      ()
    }

    it should "parse block comments in objects" in {
      Json(""" {
        /* here */ "true"  /* here */ : /* here */ true  /* and here */ ,
        /* here */ "false" /* here */ : /* here */ false /* and here */
      } """)
      ()
    }

    it should "parse block comments in arrays" in {
      Json("[ /* here */ true /* here */ , /* here */ false /* and here */ ]")
      ()
    }
  }

  def ParsesJsonLiterals(variant: JsonVariant): Unit = {
    import variant._
    import AnyForm.{ parseJson => Json }

    it should "parse empty input" in {
      Json("") should equal (NoForm)
    }

    it should "parse empty objects" in {
      Json("{ }") should equal (ObjectForm.empty)
    }

    it should "parse empty arrays" in {
      Json("[ ]") should equal (SeqForm.empty)
    }

    it should "parse empty strings" in {
      Json("\"\"") should equal (TextForm.empty)
    }

    it should "parse positive integers" in {
      Json("0")  should equal (NumberForm(0))
      Json("1")  should equal (NumberForm(1))
      Json("5")  should equal (NumberForm(5))
      Json("10") should equal (NumberForm(10))
      Json("11") should equal (NumberForm(11))
      Json("15") should equal (NumberForm(15))
    }

    it should "parse negative integers" in {
      Json("-1")  should equal (NumberForm(-1))
      Json("-5")  should equal (NumberForm(-5))
      Json("-10") should equal (NumberForm(-10))
      Json("-11") should equal (NumberForm(-11))
      Json("-15") should equal (NumberForm(-15))
    }

    it should "parse positive decimals" in {
      Json("0.0")   should equal (NumberForm(0.0))
      Json("0.5")   should equal (NumberForm(0.5))
      Json("1.0")   should equal (NumberForm(1.0))
      Json("1.5")   should equal (NumberForm(1.5))
      Json("10.0")  should equal (NumberForm(10.0))
      Json("10.5")  should equal (NumberForm(10.5))
      Json("10.00") should equal (NumberForm("10.00"))
      Json("10.50") should equal (NumberForm("10.50"))
    }

    it should "parse negative decimals" in {
      Json("-0.0")   should equal (NumberForm(-0.0))
      Json("-0.5")   should equal (NumberForm(-0.5))
      Json("-1.0")   should equal (NumberForm(-1.0))
      Json("-1.5")   should equal (NumberForm(-1.5))
      Json("-10.0")  should equal (NumberForm(-10.0))
      Json("-10.5")  should equal (NumberForm(-10.5))
      Json("-10.00") should equal (NumberForm("-10.00"))
      Json("-10.50") should equal (NumberForm("-10.50"))
    }

    it should "parse positive decimals with exponents" in {
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

    it should "parse negative decimals with exponents" in {
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

    it should "parse \"true\"" in {
      Json("true") should equal (TrueForm)
    }

    it should "parse \"false\"" in {
      Json("false") should equal (FalseForm)
    }

    it should "parse \"null\"" in {
      Json("null") should equal (NullForm)
    }

    it should "parse \"undefined\"" in {
      Json("undefined") should equal (NoForm)
    }

    it should "parse non-empty objects" in {
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
          ("string",    TextForm.empty),
          ("integer",   NumberForm(0)),
          ("decimal",   NumberForm(0.0)),
          ("true",      TrueForm),
          ("false",     FalseForm),
          ("null",      NullForm),
          ("undefined", NoForm)
        )
      )
    }

    it should "parse non-empty arrays" in {
      Json(""" [{}, [], "", 0, 0.0, true, false, null, undefined] """) should equal (
        SeqForm(
          ObjectForm.empty,
          SeqForm.empty,
          TextForm.empty,
          NumberForm(0),
          NumberForm(0.0),
          TrueForm,
          FalseForm,
          NullForm,
          NoForm
        )
      )
    }

    it should "parse objects nested in arrays" in {
      Json(""" [{ "true" : true }, { "false" : false }] """) should equal (
        SeqForm(ObjectForm("true" -> TrueForm), ObjectForm("false" -> FalseForm))
      )
    }

    it should "parse arrays nested in objects" in {
      Json(""" { "a" : [true], "b" : [false] } """) should equal (
        ObjectForm("a" -> SeqForm(TrueForm), "b" -> SeqForm(FalseForm))
      )
    }

    it should "parse non-empty strings" in {
      Json("\"test\"") should equal (TextForm("test"))
    }

    it should "unescape parsed strings" in {
      Json("\"\\\"\"") should equal (TextForm("\""))
      Json("\"\\\'\"") should equal (TextForm("\'"))
      Json("\"\\\\\"") should equal (TextForm("\\"))
      Json("\"\\/\"")  should equal (TextForm("/"))
      Json("\"\\b\"")  should equal (TextForm("\b"))
      Json("\"\\f\"")  should equal (TextForm("\f"))
      Json("\"\\n\"")  should equal (TextForm("\n"))
      Json("\"\\r\"")  should equal (TextForm("\r"))
      Json("\"\\t\"")  should equal (TextForm("\t"))
    }
  }

  def RejectsInvalidJson(variant: JsonVariant): Unit = {
    import variant._
    import AnyForm.{ parseJson => Json }

    it should "not parse sequential values" in {
      a [JsonException] should be thrownBy (Json("true false"))
      ()
    }

    it should "not parse unclosed empty objects" in {
      a [JsonException] should be thrownBy (Json("{"))
      ()
    }

    it should "not parse unclosed non-empty objects" in {
      a [JsonException] should be thrownBy (Json("{\"true\":true"))
      ()
    }

    it should "not parse objects with trailing commas" in {
      a [JsonException] should be thrownBy (Json("{\"true\":true,}"))
      ()
    }

    it should "not parse unclosed empty arrays" in {
      a [JsonException] should be thrownBy (Json("["))
      ()
    }

    it should "not parse unclosed non-empty arrays" in {
      a [JsonException] should be thrownBy (Json("[true"))
      ()
    }

    it should "not parse arrays with trailing commas" in {
      a [JsonException] should be thrownBy (Json("[true,]"))
      ()
    }

    it should "not parse unclosed empty strings" in {
      a [JsonException] should be thrownBy (Json("\""))
      ()
    }

    it should "not parse numbers with a leading zeros" in {
      withClue("\"00\"")  (a [JsonException] should be thrownBy (Json("00")))
      withClue("\"01\"")  (a [JsonException] should be thrownBy (Json("01")))
      withClue("\"-00\"") (a [JsonException] should be thrownBy (Json("-00")))
      withClue("\"-01\"") (a [JsonException] should be thrownBy (Json("-01")))
      ()
    }

    it should "not parse numbers with a trailing decimal point" in {
      withClue("\"0.\"")  (a [JsonException] should be thrownBy (Json("0.")))
      withClue("\"1.\"")  (a [JsonException] should be thrownBy (Json("1.")))
      withClue("\"-0.\"") (a [JsonException] should be thrownBy (Json("-0.")))
      withClue("\"-1.\"") (a [JsonException] should be thrownBy (Json("-1.")))
      ()
    }

    it should "not parse numbers with an invalid exponent" in {
      withClue("\"4.0e\"")  (a [JsonException] should be thrownBy (Json("4.0e")))
      withClue("\"4.0E\"")  (a [JsonException] should be thrownBy (Json("4.0E")))
      withClue("\"4.0e+\"") (a [JsonException] should be thrownBy (Json("4.0e+")))
      withClue("\"4.0E+\"") (a [JsonException] should be thrownBy (Json("4.0E+")))
      withClue("\"4.0e-\"") (a [JsonException] should be thrownBy (Json("4.0e-")))
      withClue("\"4.0E-\"") (a [JsonException] should be thrownBy (Json("4.0E-")))
      ()
    }
  }
}
