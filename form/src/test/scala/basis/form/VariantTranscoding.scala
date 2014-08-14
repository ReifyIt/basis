//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.util._
import org.scalatest._
import org.scalatest.matchers._

trait VariantTranscoding { this: FlatSpec =>
  import Matchers._

  val variant: Variant
  import variant._

  protected def transcode: Matcher[AnyForm]

  def Transcodes(): Unit = {
    it should "transcode empty objects" in {
      ObjectForm.empty should transcode
    }

    it should "transcode empty arrays" in {
      SeqForm.empty should transcode
    }

    it should "transcode empty text" in {
      TextForm.empty should transcode
    }

    it should "transcode empty data" in {
      DataForm.empty should transcode
    }

    it should "transcode positive integers" in {
      NumberForm(0) should transcode
      NumberForm(1) should transcode
      NumberForm(5) should transcode
      NumberForm(10) should transcode
      NumberForm(11) should transcode
      NumberForm(15) should transcode
    }

    it should "transcode negative integers" in {
      NumberForm(-0) should transcode
      NumberForm(-1) should transcode
      NumberForm(-5) should transcode
      NumberForm(-10) should transcode
      NumberForm(-11) should transcode
      NumberForm(-15) should transcode
    }

    it should "transcode positive decimals" in {
      NumberForm(0.0) should transcode
      NumberForm(0.5) should transcode
      NumberForm(1.0) should transcode
      NumberForm(1.5) should transcode
      NumberForm(10.0) should transcode
      NumberForm(10.5) should transcode
      NumberForm("10.00") should transcode
      NumberForm("10.50") should transcode
    }

    it should "transcode negative decimals" in {
      NumberForm(-0.0) should transcode
      NumberForm(-0.5) should transcode
      NumberForm(-1.0) should transcode
      NumberForm(-1.5) should transcode
      NumberForm(-10.0) should transcode
      NumberForm(-10.5) should transcode
      NumberForm("-10.00") should transcode
      NumberForm("-10.50") should transcode
    }

    it should "transcode positive decimals with exponents" in {
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

    it should "transcode negative decimals with exponents" in {
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

    it should "transcode dates" in {
      DateForm.now should transcode
      DateForm("2000-01-01T00:00:00.000Z") should transcode
      DateForm("2000-01-01T00:00:00.000+00:00") should transcode
      DateForm("2000-01-01T00:00:00.000-00:00") should transcode
      DateForm("2000-01-01T00:00:00Z") should transcode
      DateForm("2000-01-01T00:00:00+00:00") should transcode
      DateForm("2000-01-01T00:00:00-00:00") should transcode
      DateForm("1999-12-31T23:59:59.999Z") should transcode
      DateForm("1999-12-31T23:59:59.999+23:59") should transcode
      DateForm("1999-12-31T23:59:59.999-23:59") should transcode
      DateForm("1999-12-31T23:59:59Z") should transcode
      DateForm("1999-12-31T23:59:59+23:59") should transcode
      DateForm("1999-12-31T23:59:59-23:59") should transcode
    }

    it should "transcode boolean values" in {
      TrueForm should transcode
      FalseForm should transcode
    }

    it should "transcode null values" in {
      NullForm should transcode
    }

    it should "transcode undefined values" in {
      NoForm should transcode
    }

    it should "transcode empty objects in objects" in {
      ObjectForm("object" -> ObjectForm.empty) should transcode
    }

    it should "transcode non-empty objects in objects" in {
      ObjectForm("object" -> ObjectForm("true" -> TrueForm)) should transcode
    }

    it should "transcode empty arrays in objects" in {
      ObjectForm("array" -> SeqForm.empty) should transcode
    }

    it should "transcode non-empty arrays in objects" in {
      ObjectForm("array" -> SeqForm(TrueForm)) should transcode
    }

    it should "transcode non-empty objects" in {
      ObjectForm(
        "object" -> ObjectForm.empty,
        "array" -> SeqForm.empty,
        "text" -> TextForm.empty,
        "data" -> DataForm.empty,
        "int32" -> NumberForm(0xF7F6F5F4),
        "int64" -> NumberForm(0xF7F6F5F4F3F2F1F0L),
        "double" -> NumberForm(0.5),
        "date" -> DateForm.now,
        "true" -> TrueForm,
        "false" -> FalseForm,
        "null" -> NullForm
      ) should transcode
    }

    it should "transcode non-empty arrays" in {
      SeqForm(
        ObjectForm.empty,
        SeqForm.empty,
        TextForm.empty,
        DataForm.empty,
        NumberForm(0xF7F6F5F4),
        NumberForm(0xF7F6F5F4F3F2F1F0L),
        NumberForm(0.5),
        DateForm.now,
        TrueForm,
        FalseForm,
        NullForm
      ) should transcode
    }

    it should "transcode objects nested in arrays" in {
      SeqForm(ObjectForm("true" -> TrueForm), ObjectForm("false" -> FalseForm)) should transcode
    }

    it should "transcode arrays nested in objects" in {
      ObjectForm("a" -> SeqForm(TrueForm), "b" -> SeqForm(FalseForm)) should transcode
    }

    it should "transcode non-empty text" in {
      TextForm("test") should transcode
    }

    it should "transcode non-empty data" in {
      DataForm.fromBase64("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/-_") should transcode
    }

    it should "transcode escaped characters" in {
      TextForm("\"") should transcode
      TextForm("\\") should transcode
      TextForm("\b") should transcode
      TextForm("\f") should transcode
      TextForm("\n") should transcode
      TextForm("\r") should transcode
      TextForm("\t") should transcode
    }

    it should "transcode Unicode strings" in {
      withClue("U+0000:")   (TextForm("\u0000") should transcode)
      withClue("U+007F:")   (TextForm("\u007F") should transcode)
      withClue("U+0080:")   (TextForm("\u0080") should transcode)
      withClue("U+07FF:")   (TextForm("\u07FF") should transcode)
      withClue("U+0800:")   (TextForm("\u0800") should transcode)
      withClue("U+0FFF:")   (TextForm("\u0FFF") should transcode)
      withClue("U+1000:")   (TextForm("\u1000") should transcode)
      withClue("U+CFFF:")   (TextForm("\uCFFF") should transcode)
      withClue("U+D000:")   (TextForm("\uD000") should transcode)
      withClue("U+D7FF:")   (TextForm("\uD7FF") should transcode)
      withClue("U+E000:")   (TextForm("\uE000") should transcode)
      withClue("U+FFFF:")   (TextForm("\uFFFF") should transcode)
      withClue("U+10000:")  (TextForm("\uD800\uDC00") should transcode)
      withClue("U+3FFFF:")  (TextForm("\uD8BF\uDFFF") should transcode)
      withClue("U+40000:")  (TextForm("\uD8C0\uDC00") should transcode)
      withClue("U+FFFFF:")  (TextForm("\uDBBF\uDFFF") should transcode)
      withClue("U+100000:") (TextForm("\uDBC0\uDC00") should transcode)
      withClue("U+10FFFF:") (TextForm("\uDBFF\uDFFF") should transcode)
    }
  }
}
