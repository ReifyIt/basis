//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.util._
import org.scalatest._
import org.scalatest.matchers._

trait JsonInterpolatorBehaviors { this: FunSpec =>
  import ShouldMatchers._

  val variant: JsonVariant
  import variant._

  def InterpolatesJsonComments() = describe("interpolating json\"\" comments") {
    it("should parse preceding line comments") {
      json"""// comment
      true"""
    }

    it("should parse succeeding line comments") {
      json"true // comment"
    }

    it("should parse preceding block comments") {
      json"/* comment */ true"
    }

    it("should parse succeeding block comments") {
      json"true /* comment */"
    }

    it("should parse line comments in objects") {
      json""" { // here
        "true"  // here
        :       // here
        true    // here
        ,       // here
        "false" // here
        :       // here
        false   // and here
      } """
    }

    it("should parse line comments in arrays") {
      json""" [ // here
      true      // here
      ,         // here
      false     // and here
      ] """
    }

    it("should parse block comments in objects") {
      json""" {
        /* here */ "true"  /* here */ : /* here */ true  /* and here */ ,
        /* here */ "false" /* here */ : /* here */ false /* and here */
      } """
    }

    it("should parse block comments in arrays") {
      json"[ /* here */ true /* here */ , /* here */ false /* and here */ ]"
    }
  }

  def InterpolatesJsonLiterals() = describe("interpolating json\"\" literals") {
    it("should parse empty objects") {
      json"{}" should equal (ObjectForm.empty)
    }

    it("should parse empty arrays") {
      json"[]" should equal (SeqForm.empty)
    }

    it("should parse empty strings") {
      json""" "" """ should equal (StringForm.empty)
    }

    it("should parse positive integers") {
      json"0"  should equal (NumberForm(0))
      json"1"  should equal (NumberForm(1))
      json"5"  should equal (NumberForm(5))
      json"10" should equal (NumberForm(10))
      json"11" should equal (NumberForm(11))
      json"15" should equal (NumberForm(15))
    }

    it("should parse negative integers") {
      json"-0"  should equal (NumberForm(-0))
      json"-1"  should equal (NumberForm(-1))
      json"-5"  should equal (NumberForm(-5))
      json"-10" should equal (NumberForm(-10))
      json"-11" should equal (NumberForm(-11))
      json"-15" should equal (NumberForm(-15))
    }

    it("should parse positive decimals") {
      json"0.0"   should equal (NumberForm(0.0))
      json"0.5"   should equal (NumberForm(0.5))
      json"1.0"   should equal (NumberForm(1.0))
      json"1.5"   should equal (NumberForm(1.5))
      json"10.0"  should equal (NumberForm(10.0))
      json"10.5"  should equal (NumberForm(10.5))
    }

    it("should parse negative decimals") {
      json"-0.0"   should equal (NumberForm(-0.0))
      json"-0.5"   should equal (NumberForm(-0.5))
      json"-1.0"   should equal (NumberForm(-1.0))
      json"-1.5"   should equal (NumberForm(-1.5))
      json"-10.0"  should equal (NumberForm(-10.0))
      json"-10.5"  should equal (NumberForm(-10.5))
    }

    it("should parse positive decimals with exponents") {
      json"4e2"    should equal (NumberForm(400.0))
      json"4E2"    should equal (NumberForm(400.0))
      json"4e+2"   should equal (NumberForm(400.0))
      json"4E+2"   should equal (NumberForm(400.0))
      json"4e-2"   should equal (NumberForm(0.04))
      json"4E-2"   should equal (NumberForm(0.04))
      json"4.0e2"  should equal (NumberForm(400.0))
      json"4.0E2"  should equal (NumberForm(400.0))
      json"4.0e+2" should equal (NumberForm(400.0))
      json"4.0E+2" should equal (NumberForm(400.0))
      json"4.0e-2" should equal (NumberForm(0.04))
      json"4.0E-2" should equal (NumberForm(0.04))
    }

    it("should parse negative decimals with exponents") {
      json"-4e2"    should equal (NumberForm(-400.0))
      json"-4E2"    should equal (NumberForm(-400.0))
      json"-4e+2"   should equal (NumberForm(-400.0))
      json"-4E+2"   should equal (NumberForm(-400.0))
      json"-4e-2"   should equal (NumberForm(-0.04))
      json"-4E-2"   should equal (NumberForm(-0.04))
      json"-4.0e2"  should equal (NumberForm(-400.0))
      json"-4.0E2"  should equal (NumberForm(-400.0))
      json"-4.0e+2" should equal (NumberForm(-400.0))
      json"-4.0E+2" should equal (NumberForm(-400.0))
      json"-4.0e-2" should equal (NumberForm(-0.04))
      json"-4.0E-2" should equal (NumberForm(-0.04))
    }

    it("should parse \"true\"") {
      json"true" should equal (TrueForm)
    }

    it("should parse \"false\"") {
      json"false" should equal (FalseForm)
    }

    it("should parse \"null\"") {
      json"null" should equal (NullForm)
    }

    it("should parse \"undefined\"") {
      json"undefined" should equal (UndefinedForm)
    }

    it("should orderly parse non-empty objects") {
      json""" {
        "object"    : {},
        "array"     : [],
        "string"    : "",
        "integer"   : 0,
        "decimal"   : 0.0,
        "true"      : true,
        "false"     : false,
        "null"      : null,
        "undefined" : undefined
      } """ should equal (
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
      json""" [{}, [], "", 0, 0.0, true, false, null, undefined] """ should equal (
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
      json""" [{ "true" : true }, { "false" : false }] """ should equal {
        SeqForm(ObjectForm("true" -> TrueForm), ObjectForm("false" -> FalseForm))
      }
    }

    it("should parse arrays nested in objects") {
      json""" { "a" : [true], "b" : [false] } """ should equal {
        ObjectForm("a" -> SeqForm(TrueForm), "b" -> SeqForm(FalseForm))
      }
    }

    it("should parse non-empty strings") {
      json""" "test" """ should equal (StringForm("test"))
    }

    it("should unescape parsed strings") {
      json""" " \" " """ should equal (StringForm(""" " """))
      json""" " \' " """ should equal (StringForm(""" ' """))
      json""" " \\ " """ should equal (StringForm(""" \ """))
      json""" " \/ " """ should equal (StringForm(""" / """))
      json""" " \b " """ should equal (StringForm(" \b "))
      json""" " \f " """ should equal (StringForm(" \f "))
      json""" " \n " """ should equal (StringForm(" \n "))
      json""" " \r " """ should equal (StringForm(" \r "))
      json""" " \t " """ should equal (StringForm(" \t "))
    }
  }

  def InterpolatesJsonArguments() = describe("interpolating json\"\" arguments") {
    it("should interpolate ObjectForm") {
      val x = ObjectForm.empty
      json"$x" should equal (x)
    }

    it("should interpolate SeqForm") {
      val x = SeqForm.empty
      json"$x" should equal (x)
    }

    it("should interpolate StringForm") {
      val x = StringForm.empty
      json"$x" should equal (x)
    }

    it("should interpolate NumberForm") {
      val x = NumberForm(0.0)
      json"$x" should equal (x)
    }

    it("should interpolate BooleanForm") {
      val x = TrueForm
      json"$x" should equal (x)
    }

    it("should interpolate NullForm") {
      val x = NullForm
      json"$x" should equal (x)
    }

    it("should interpolate UndefinedForm") {
      val x = UndefinedForm
      json"$x" should equal (x)
    }

    it("should interpolate String like StringForm") {
      val x = ""
      (json"$x": AnyForm) should equal (StringForm.empty)
    }

    it("should interpolate Int like NumberForm") {
      val x = 0
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it("should interpolate Long like NumberForm") {
      val x = 0L
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it("should interpolate Float like NumberForm") {
      val x = 0.0F
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it("should interpolate Double like NumberForm") {
      val x = 0.0
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it("should interpolate Boolean like BooleanForm") {
      val x = true
      (json"$x": AnyForm) should equal (TrueForm)
    }

    it("should interpolate values into objects") {
      json""" {
        "object"    : ${ObjectForm.empty},
        "array"     : ${SeqForm.empty},
        "string"    : ${StringForm.empty},
        "integer"   : ${NumberForm(0)},
        "decimal"   : ${NumberForm(0.0)},
        "true"      : $TrueForm,
        "false"     : $FalseForm,
        "null"      : $NullForm,
        "undefined" : $UndefinedForm
      } """ should equal (
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

    it("should interpolate values into arrays") {
      json""" [
        ${ObjectForm.empty},
        ${SeqForm.empty},
        ${StringForm.empty},
        ${NumberForm(0)},
        ${NumberForm(0.0)},
        $TrueForm,
        $FalseForm,
        $NullForm,
        $UndefinedForm
      ] """ should equal (
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
  }
}
