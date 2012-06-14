/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class JSONStringContextSpec extends FunSpec with ShouldMatchers {
  override def suiteName = "JSONStringContext specification"
  
  describe("json\"\" literals") {
    it("should parse empty objects") {
      json" { } " should equal (JSObject.empty)
    }
    
    it("should parse empty arrays") {
      json" [ ] " should equal (JSArray.empty)
    }
    
    it("should parse empty strings") {
      json""" "" """ should equal (JSString.empty)
    }
    
    it("should parse positive integers") {
      json" 0 "  should equal (JSInteger(0))
      json" 1 "  should equal (JSInteger(1))
      json" 5 "  should equal (JSInteger(5))
      json" 10 " should equal (JSInteger(10))
      json" 11 " should equal (JSInteger(11))
      json" 15 " should equal (JSInteger(15))
    }
    
    it("should parse negative integers") {
      json" -0 "  should equal (JSInteger(-0))
      json" -1 "  should equal (JSInteger(-1))
      json" -5 "  should equal (JSInteger(-5))
      json" -10 " should equal (JSInteger(-10))
      json" -11 " should equal (JSInteger(-11))
      json" -15 " should equal (JSInteger(-15))
    }
    
    it("should parse positive decimals") {
      json" 0.0 "   should equal (JSDecimal(0.0))
      json" 0.5 "   should equal (JSDecimal(0.5))
      json" 1.0 "   should equal (JSDecimal(1.0))
      json" 1.5 "   should equal (JSDecimal(1.5))
      json" 10.0 "  should equal (JSDecimal(10.0))
      json" 10.5 "  should equal (JSDecimal(10.5))
      json" 10.00 " should equal (JSDecimal(10.00))
      json" 10.50 " should equal (JSDecimal(10.50))
    }
    
    it("should parse negative decimals") {
      json" -0.0 "   should equal (JSDecimal(-0.0))
      json" -0.5 "   should equal (JSDecimal(-0.5))
      json" -1.0 "   should equal (JSDecimal(-1.0))
      json" -1.5 "   should equal (JSDecimal(-1.5))
      json" -10.0 "  should equal (JSDecimal(-10.0))
      json" -10.5 "  should equal (JSDecimal(-10.5))
      json" -10.00 " should equal (JSDecimal(-10.00))
      json" -10.50 " should equal (JSDecimal(-10.50))
    }
    
    it("should parse positive decimals with exponents") {
      json" 4e2 "    should equal (JSDecimal(4e2))
      json" 4E2 "    should equal (JSDecimal(4E2))
      json" 4e+2 "   should equal (JSDecimal(4e+2))
      json" 4E+2 "   should equal (JSDecimal(4E+2))
      json" 4e-2 "   should equal (JSDecimal(4e-2))
      json" 4E-2 "   should equal (JSDecimal(4E-2))
      json" 4.0e2 "  should equal (JSDecimal(4.0e2))
      json" 4.0E2 "  should equal (JSDecimal(4.0E2))
      json" 4.0e+2 " should equal (JSDecimal(4.0e+2))
      json" 4.0E+2 " should equal (JSDecimal(4.0E+2))
      json" 4.0e-2 " should equal (JSDecimal(4.0e-2))
      json" 4.0E-2 " should equal (JSDecimal(4.0E-2))
    }
    
    it("should parse negative decimals with exponents") {
      json" -4e2 "    should equal (JSDecimal(-4e2))
      json" -4E2 "    should equal (JSDecimal(-4E2))
      json" -4e+2 "   should equal (JSDecimal(-4e+2))
      json" -4E+2 "   should equal (JSDecimal(-4E+2))
      json" -4e-2 "   should equal (JSDecimal(-4e-2))
      json" -4E-2 "   should equal (JSDecimal(-4E-2))
      json" -4.0e2 "  should equal (JSDecimal(-4.0e2))
      json" -4.0E2 "  should equal (JSDecimal(-4.0E2))
      json" -4.0e+2 " should equal (JSDecimal(-4.0e+2))
      json" -4.0E+2 " should equal (JSDecimal(-4.0E+2))
      json" -4.0e-2 " should equal (JSDecimal(-4.0e-2))
      json" -4.0E-2 " should equal (JSDecimal(-4.0E-2))
    }
    
    it("should parse \"true\"") {
      json" true " should equal (JSTrue)
    }
    
    it("should parse \"false\"") {
      json" false " should equal (JSFalse)
    }
    
    it("should parse \"null\"") {
      json" null " should equal (JSNull)
    }
    
    it("should orderly parse non-empty objects") {
      json""" {
        "object"  : {},
        "array"   : [],
        "string"  : "",
        "integer" : 0,
        "decimal" : 0.0,
        "true"    : true,
        "false"   : false,
        "null"    : null
      } """ should equal {
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
      json""" [{}, [], "", 0, 0.0, true, false, null] """ should equal {
        JSArray(JSObject.empty, JSArray.empty, JSString.empty,
        JSInteger(0), JSDecimal(0.0), JSTrue, JSFalse, JSNull)
      }
    }
    
    it("should parse objects nested in arrays") {
      json""" [{ "true" : true }, { "false" : false }] """ should equal {
        JSArray(JSObject("true" -> JSTrue), JSObject("false" -> JSFalse))
      }
    }
    
    it("should parse arrays nested in objects") {
      json""" { "a" : [true], "b" : [false] } """ should equal {
        JSObject("a" -> JSArray(JSTrue), "b" -> JSArray(JSFalse))
      }
    }
    
    it("should parse non-empty strings") {
      json""" "test" """ should equal (JSString("test"))
    }
    
    it("should unescape parsed strings") {
      json""" " \" " """ should equal (JSString(""" " """))
      json""" " \' " """ should equal (JSString(""" ' """))
      json""" " \\ " """ should equal (JSString(""" \ """))
      json""" " \/ " """ should equal (JSString(""" / """))
      json""" " \b " """ should equal (JSString(" \b "))
      json""" " \f " """ should equal (JSString(" \f "))
      json""" " \n " """ should equal (JSString(" \n "))
      json""" " \r " """ should equal (JSString(" \r "))
      json""" " \t " """ should equal (JSString(" \t "))
    }
  }
  
  describe("json\"\" interpolation") {
    it("should interpolate JSObject") {
      val x = JSObject.empty
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate JSArray") {
      val x = JSArray.empty
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate JSString") {
      val x = JSString.empty
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate JSInteger") {
      val x = JSInteger(1)
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate JSDecimal") {
      val x = JSDecimal(1.0)
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate JSBoolean") {
      val x = JSTrue
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate JSNull") {
      val x = JSNull
      json"$x"   should equal (x)
      json" $x " should equal (x)
    }
    
    it("should interpolate Map[String, JSValue] like JSObject") {
      val x = Map.empty[String, JSValue]
      (json"$x": Any) should equal (JSObject.empty)
    }
    
    it("should interpolate Seq[JSValue] like JSArray") {
      val x = Seq.empty[JSValue]
      (json"$x": Any) should equal (JSArray.empty)
    }
    
    it("should interpolate String like JSString") {
      val x = ""
      (json"$x": Any) should equal (JSString.empty)
    }
    
    it("should interpolate Int like JSInteger") {
      val x = 0
      (json"$x": Any) should equal (JSInteger(x))
    }
    
    it("should interpolate Long like JSInteger") {
      val x = 0L
      (json"$x": Any) should equal (JSInteger(x))
    }
    
    it("should interpolate Float like JSDecimal") {
      val x = 0.0F
      (json"$x": Any) should equal (JSDecimal(x))
    }
    
    it("should interpolate Double like JSDecimal") {
      val x = 0.0
      (json"$x": Any) should equal (JSDecimal(x))
    }
    
    it("should interpolate Boolean like JSBoolean") {
      val x = true
      (json"$x": Any) should equal (JSTrue)
    }
    
    it("should interpolate values into objects") {
      json""" {
        "object"  : ${JSObject.empty},
        "array"   : ${JSArray.empty},
        "string"  : ${JSString.empty},
        "integer" : ${JSInteger(0)},
        "decimal" : ${JSDecimal(0.0)},
        "true"    : $JSTrue,
        "false"   : $JSFalse,
        "null"    : $JSNull
      } """ should equal {
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
    
    it("should interpolate values into arrays") {
      json""" [
        ${JSObject.empty}, ${JSArray.empty}, ${JSString.empty},
        ${JSInteger(0)}, ${JSDecimal(0.0)}, $JSTrue, $JSFalse, $JSNull
      ] """ should equal {
        JSArray(JSObject.empty, JSArray.empty, JSString.empty,
        JSInteger(0), JSDecimal(0.0), JSTrue, JSFalse, JSNull)
      }
    }
  }
}
