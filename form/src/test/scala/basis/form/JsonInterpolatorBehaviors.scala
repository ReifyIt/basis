//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.util._
import org.scalatest._

trait JsonInterpolatorBehaviors extends Matchers { this: FlatSpec =>
  def InterpolatesJsonComments(variant: JsonVariant): Unit = {
    import variant._

    it should "interpolate preceding line comments" in {
      json"""// comment
      true"""
      ()
    }

    it should "interpolate succeeding line comments" in {
      json"true // comment"
      ()
    }

    it should "interpolate preceding block comments" in {
      json"/* comment */ true"
      ()
    }

    it should "interpolate succeeding block comments" in {
      json"true /* comment */"
      ()
    }

    it should "interpolate line comments in objects" in {
      json""" { // here
        "true"  // here
        :       // here
        true    // here
        ,       // here
        "false" // here
        :       // here
        false   // and here
      } """
      ()
    }

    it should "interpolate line comments in arrays" in {
      json""" [ // here
      true      // here
      ,         // here
      false     // and here
      ] """
      ()
    }

    it should "interpolate block comments in objects" in {
      json""" {
        /* here */ "true"  /* here */ : /* here */ true  /* and here */ ,
        /* here */ "false" /* here */ : /* here */ false /* and here */
      } """
      ()
    }

    it should "interpolate block comments in arrays" in {
      json"[ /* here */ true /* here */ , /* here */ false /* and here */ ]"
      ()
    }
  }

  def InterpolatesJsonLiterals(variant: JsonVariant): Unit = {
    import variant._

    it should "interpolate empty objects" in {
      json"{}" should equal (ObjectForm.empty)
    }

    it should "interpolate empty arrays" in {
      json"[]" should equal (SeqForm.empty)
    }

    it should "interpolate empty strings" in {
      json""" "" """ should equal (TextForm.empty)
    }

    it should "interpolate positive integers" in {
      json"0"  should equal (NumberForm(0))
      json"1"  should equal (NumberForm(1))
      json"5"  should equal (NumberForm(5))
      json"10" should equal (NumberForm(10))
      json"11" should equal (NumberForm(11))
      json"15" should equal (NumberForm(15))
    }

    it should "interpolate negative integers" in {
      json"-0"  should equal (NumberForm(-0))
      json"-1"  should equal (NumberForm(-1))
      json"-5"  should equal (NumberForm(-5))
      json"-10" should equal (NumberForm(-10))
      json"-11" should equal (NumberForm(-11))
      json"-15" should equal (NumberForm(-15))
    }

    it should "interpolate positive decimals" in {
      json"0.0"   should equal (NumberForm(0.0))
      json"0.5"   should equal (NumberForm(0.5))
      json"1.0"   should equal (NumberForm(1.0))
      json"1.5"   should equal (NumberForm(1.5))
      json"10.0"  should equal (NumberForm(10.0))
      json"10.5"  should equal (NumberForm(10.5))
    }

    it should "interpolate negative decimals" in {
      json"-0.0"   should equal (NumberForm(-0.0))
      json"-0.5"   should equal (NumberForm(-0.5))
      json"-1.0"   should equal (NumberForm(-1.0))
      json"-1.5"   should equal (NumberForm(-1.5))
      json"-10.0"  should equal (NumberForm(-10.0))
      json"-10.5"  should equal (NumberForm(-10.5))
    }

    it should "interpolate positive decimals with exponents" in {
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

    it should "interpolate negative decimals with exponents" in {
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

    it should "interpolate \"true\"" in {
      json"true" should equal (TrueForm)
    }

    it should "interpolate \"false\"" in {
      json"false" should equal (FalseForm)
    }

    it should "interpolate \"null\"" in {
      json"null" should equal (NullForm)
    }

    it should "interpolate \"undefined\"" in {
      json"undefined" should equal (NoForm)
    }

    it should "interpolate non-empty objects in order" in {
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

    it should "interpolate non-empty arrays" in {
      json""" [{}, [], "", 0, 0.0, true, false, null, undefined] """ should equal (
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

    it should "interpolate objects nested in arrays" in {
      json""" [{ "true" : true }, { "false" : false }] """ should equal {
        SeqForm(ObjectForm("true" -> TrueForm), ObjectForm("false" -> FalseForm))
      }
    }

    it should "interpolate arrays nested in objects" in {
      json""" { "a" : [true], "b" : [false] } """ should equal {
        ObjectForm("a" -> SeqForm(TrueForm), "b" -> SeqForm(FalseForm))
      }
    }

    it should "interpolate non-empty strings" in {
      json""" "test" """ should equal (TextForm("test"))
    }

    it should "unescape interpolated strings" in {
      json""" " \" " """ should equal (TextForm(""" " """))
      json""" " \' " """ should equal (TextForm(""" ' """))
      json""" " \\ " """ should equal (TextForm(""" \ """))
      json""" " \/ " """ should equal (TextForm(""" / """))
      json""" " \b " """ should equal (TextForm(" \b "))
      json""" " \f " """ should equal (TextForm(" \f "))
      json""" " \n " """ should equal (TextForm(" \n "))
      json""" " \r " """ should equal (TextForm(" \r "))
      json""" " \t " """ should equal (TextForm(" \t "))
    }
  }

  def InterpolatesJsonArguments(variant: JsonVariant): Unit = {
    import variant._

    it should "interpolate ObjectForm variables" in {
      val x = ObjectForm.empty
      json"$x" should equal (x)
    }

    it should "interpolate SeqForm variables" in {
      val x = SeqForm.empty
      json"$x" should equal (x)
    }

    it should "interpolate TextForm variables" in {
      val x = TextForm.empty
      json"$x" should equal (x)
    }

    it should "interpolate NumberForm variables" in {
      val x = NumberForm(0.0)
      json"$x" should equal (x)
    }

    it should "interpolate BoolForm variables" in {
      val x = TrueForm
      json"$x" should equal (x)
    }

    it should "interpolate NullForm variables" in {
      val x = NullForm
      json"$x" should equal (x)
    }

    it should "interpolate NoForm variables" in {
      val x = NoForm
      json"$x" should equal (x)
    }

/*  // Implicit conversions to path-dependent types don't currently work.
    it should "interpolate String variables as TextForm values" in {
      val x = ""
      (json"$x": AnyForm) should equal (TextForm.empty)
    }

    it should "interpolate Int variables as NumberForm values" in {
      val x = 0
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it should "interpolate Long variables as NumberForm values" in {
      val x = 0L
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it should "interpolate Float variables as NumberForm values" in {
      val x = 0.0F
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it should "interpolate Double variables as NumberForm values" in {
      val x = 0.0
      (json"$x": AnyForm) should equal (NumberForm(x))
    }

    it should "interpolate Boolean variables as BoolForm values" in {
      val x = true
      (json"$x": AnyForm) should equal (TrueForm)
    }
*/
    it should "interpolate values into objects" in {
      json""" {
        "object"    : ${ObjectForm.empty},
        "array"     : ${SeqForm.empty},
        "string"    : ${TextForm.empty},
        "integer"   : ${NumberForm(0)},
        "decimal"   : ${NumberForm(0.0)},
        "true"      : $TrueForm,
        "false"     : $FalseForm,
        "null"      : $NullForm,
        "undefined" : $NoForm
      } """ should equal (
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

    it should "interpolate values into arrays" in {
      json""" [
        ${ObjectForm.empty},
        ${SeqForm.empty},
        ${TextForm.empty},
        ${NumberForm(0)},
        ${NumberForm(0.0)},
        $TrueForm,
        $FalseForm,
        $NullForm,
        $NoForm
      ] """ should equal (
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
  }
}
