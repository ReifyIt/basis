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

trait JsonVariantBehaviors { this: FunSpec =>
  import ShouldMatchers._

  val variant: JsonVariant
  import variant._

  def TranscodesJson() = describe("transcoding variant forms to JSON") {
    it("should transcode empty objects") {
      ObjectForm.empty should transcode
    }

    it("should transcode empty arrays") {
      SeqForm.empty should transcode
    }

    it("should transcode empty binary data") {
      BinaryForm.empty should transcode
    }

    it("should transcode empty strings") {
      StringForm.empty should transcode
    }

    it("should transcode positive integers") {
      NumberForm(0) should transcode
      NumberForm(1) should transcode
      NumberForm(5) should transcode
      NumberForm(10) should transcode
      NumberForm(11) should transcode
      NumberForm(15) should transcode
    }

    it("should transcode negative integers") {
      NumberForm(-0) should transcode
      NumberForm(-1) should transcode
      NumberForm(-5) should transcode
      NumberForm(-10) should transcode
      NumberForm(-11) should transcode
      NumberForm(-15) should transcode
    }

    it("should transcode positive decimals") {
      NumberForm(0.0) should transcode
      NumberForm(0.5) should transcode
      NumberForm(1.0) should transcode
      NumberForm(1.5) should transcode
      NumberForm(10.0) should transcode
      NumberForm(10.5) should transcode
      NumberForm("10.00") should transcode
      NumberForm("10.50") should transcode
    }

    it("should transcode negative decimals") {
      NumberForm(-0.0) should transcode
      NumberForm(-0.5) should transcode
      NumberForm(-1.0) should transcode
      NumberForm(-1.5) should transcode
      NumberForm(-10.0) should transcode
      NumberForm(-10.5) should transcode
      NumberForm("-10.00") should transcode
      NumberForm("-10.50") should transcode
    }

    it("should transcode positive decimals with exponents") {
      NumberForm("4e2") should transcode
      NumberForm("4E2") should transcode
      NumberForm("4e+2") should transcode
      NumberForm("4E+2") should transcode
      NumberForm("4e-2") should transcode
      NumberForm("4E-2") should transcode
      NumberForm("4.0e2") should transcode
      NumberForm("4.0E2") should transcode
      NumberForm("4.0e+2") should transcode
      NumberForm("4.0E+2") should transcode
      NumberForm("4.0e-2") should transcode
      NumberForm("4.0E-2") should transcode
    }

    it("should transcode negative decimals with exponents") {
      NumberForm("-4e2") should transcode
      NumberForm("-4E2") should transcode
      NumberForm("-4e+2") should transcode
      NumberForm("-4E+2") should transcode
      NumberForm("-4e-2") should transcode
      NumberForm("-4E-2") should transcode
      NumberForm("-4.0e2") should transcode
      NumberForm("-4.0E2") should transcode
      NumberForm("-4.0e+2") should transcode
      NumberForm("-4.0E+2") should transcode
      NumberForm("-4.0e-2") should transcode
      NumberForm("-4.0E-2") should transcode
    }

    it("should transcode dates") {
      DateForm.now should transcode
    }

    it("should transcode boolean values") {
      TrueForm should transcode
      FalseForm should transcode
    }

    it("should transcode null values") {
      NullForm should transcode
    }

    it("should transcode undefined values") {
      UndefinedForm should transcode
    }

    it("should transcode non-empty objects") {
      ObjectForm(
        ("object", ObjectForm.empty),
        ("array",  SeqForm.empty),
        ("binary", BinaryForm.empty),
        ("string", StringForm.empty),
        ("int32",  NumberForm(0xF7F6F5F4)),
        ("int64",  NumberForm(0xF7F6F5F4F3F2F1F0L)),
        ("double", NumberForm(0.5)),
        ("date",   DateForm.now),
        ("true",   TrueForm),
        ("false",  FalseForm),
        ("null",   NullForm)
      ) should transcode
    }

    it("should transcode non-empty arrays") {
      SeqForm(
        ObjectForm.empty,
        SeqForm.empty,
        BinaryForm.empty,
        StringForm.empty,
        NumberForm(0xF7F6F5F4),
        NumberForm(0xF7F6F5F4F3F2F1F0L),
        NumberForm(0.5),
        DateForm.now,
        TrueForm,
        FalseForm,
        NullForm
      ) should transcode
    }

    it("should transcode objects nested in arrays") {
      SeqForm(ObjectForm("true" -> TrueForm), ObjectForm("false" -> FalseForm)) should transcode
    }

    it("should transcode arrays nested in objects") {
      ObjectForm("a" -> SeqForm(TrueForm), "b" -> SeqForm(FalseForm)) should transcode
    }

    it("should transcode non-empty strings") {
      StringForm("test") should transcode
    }

    it("should transcode non-empty binary data") {
      BinaryForm("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/-_") should transcode
    }

    it("should transcode escaped characters") {
      StringForm("\"") should transcode
      StringForm("\\") should transcode
      StringForm("\b") should transcode
      StringForm("\f") should transcode
      StringForm("\n") should transcode
      StringForm("\r") should transcode
      StringForm("\t") should transcode
    }

    it("should transcode unicode strings") {
      withClue("U+0000")   (StringForm("\u0000") should transcode)
      withClue("U+007F")   (StringForm("\u007F") should transcode)
      withClue("U+0080")   (StringForm("\u0080") should transcode)
      withClue("U+07FF")   (StringForm("\u07FF") should transcode)
      withClue("U+0800")   (StringForm("\u0800") should transcode)
      withClue("U+0FFF")   (StringForm("\u0FFF") should transcode)
      withClue("U+1000")   (StringForm("\u1000") should transcode)
      withClue("U+CFFF")   (StringForm("\uCFFF") should transcode)
      withClue("U+D000")   (StringForm("\uD000") should transcode)
      withClue("U+D7FF")   (StringForm("\uD7FF") should transcode)
      withClue("U+E000")   (StringForm("\uE000") should transcode)
      withClue("U+FFFF")   (StringForm("\uFFFF") should transcode)
      withClue("U+10000")  (StringForm("\uD800\uDC00") should transcode)
      withClue("U+3FFFF")  (StringForm("\uD8BF\uDFFF") should transcode)
      withClue("U+40000")  (StringForm("\uD8C0\uDC00") should transcode)
      withClue("U+FFFFF")  (StringForm("\uDBBF\uDFFF") should transcode)
      withClue("U+100000") (StringForm("\uDBC0\uDC00") should transcode)
      withClue("U+10FFFF") (StringForm("\uDBFF\uDFFF") should transcode)
    }
  }


  private object transcode extends Matcher[AnyForm] {
    private def transcoded(x: AnyForm): AnyForm = AnyForm.parseJson(x.toJson)
    def apply(x: AnyForm): MatchResult = {
      val y = transcoded(x)
      MatchResult(
        x == y,
        s"$x improperly transcoded to $y",
        s"$x properly transcoded")
    }
  }
}
