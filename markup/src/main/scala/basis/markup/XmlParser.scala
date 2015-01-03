//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.markup

import basis.collections._
import scala.annotation._

private[markup] abstract class XmlParser[X <: XmlParsee](val xml: X) extends Iteratee[Int, X#State] {
  import XmlParser._

  protected def newXmlParsee(): X

  protected def newXmlParser(xml: X): XmlParser[X]

  protected def isSpace(c: Int): Boolean =
    c == 0x20 || c == 0x09 || c == 0x0D || c == 0x0A

  protected def isChar(c: Int): Boolean =
    c == 0x9 || c == 0xA || c == 0xD ||
    c >= 0x20 && c <= 0xD7FF ||
    c >= 0xE000 && c <= 0xFFFD ||
    c >= 0x10000 && c <= 0x10FFFF

  protected def isText(c: Int): Boolean =
    c != '&' && c != '<' && isChar(c)

  protected def isNameStartChar(c: Int): Boolean =
    c == ':' ||
    c >= 'A' && c <= 'Z' ||
    c == '_' ||
    c >= 'a' && c <= 'z' ||
    c >= 0xC0 && c <= 0xD6 ||
    c >= 0xD8 && c <= 0xF6 ||
    c >= 0xF8 && c <= 0x2FF ||
    c >= 0x370 && c <= 0x37D ||
    c >= 0x37F && c <= 0x1FFF ||
    c >= 0x200C && c <= 0x200D ||
    c >= 0x2070 && c <= 0x218F ||
    c >= 0x2C00 && c <= 0x2FEF ||
    c >= 0x3001 && c <= 0xD7FF ||
    c >= 0xF900 && c <= 0xFDCF ||
    c >= 0xFDF0 && c <= 0xFFFD ||
    c >= 0x10000 && c <= 0xEFFFF

  protected def isNameChar(c: Int): Boolean =
    c == '-' || c == '.' ||
    c >= '0' && c <= ':' ||
    c >= 'A' && c <= 'Z' ||
    c == '_' ||
    c >= 'a' && c <= 'z' ||
    c == 0xB7 ||
    c >= 0xC0 && c <= 0xD6 ||
    c >= 0xD8 && c <= 0xF6 ||
    c >= 0xF8 && c <= 0x37D ||
    c >= 0x37F && c <= 0x1FFF ||
    c >= 0x200C && c <= 0x200D ||
    c >= 0x203F && c <= 0x2040 ||
    c >= 0x2070 && c <= 0x218F ||
    c >= 0x2C00 && c <= 0x2FEF ||
    c >= 0x3001 && c <= 0xD7FF ||
    c >= 0xF900 && c <= 0xFDCF ||
    c >= 0xFDF0 && c <= 0xFFFD ||
    c >= 0x10000 && c <= 0xEFFFF

  override def feed(input: Iterator[Int]): Iteratee[Int, X#State] = {
    val xml = if (this.xml ne null) this.xml else newXmlParsee()
    while (!input.isEmpty && xml.accept > 0) (xml.accept: @switch) match {
      case StartTagName     => parseStartTagName(input, xml)
      case StartTagNameRest => parseStartTagNameRest(input, xml)
      case EndTagName       => parseEndTagName(input, xml)
      case EndTagNameRest   => parseEndTagNameRest(input, xml)
      case AttrName         => parseAttrName(input, xml)
      case AttrNameRest     => parseAttrNameRest(input, xml)
      case AttrRest         => parseAttrRest(input, xml)
      case AttrValue        => parseAttrValue(input, xml)
      case AttrValueDQRest  => parseAttrValueDQRest(input, xml)
      case AttrValueSQRest  => parseAttrValueSQRest(input, xml)
      case CommentRest      => ()
      case StartTagRest     => parseStartTagRest(input, xml)
      case StartTagSpace    => parseStartTagSpace(input, xml)
      case EmptyTagRest     => parseEmptyTagRest(input, xml)
      case EndTagRest       => parseEndTagRest(input, xml)
      case Content          => parseContent(input, xml)
      case AngleBracketRest => parseAngleBracketRest(input, xml)
      case _                => ()
    }
    if (input.isDone && xml.accept == Content) Iteratee.done(xml.state)
    else if (xml.accept == -1 && !input.isEmpty || input.isDone) Iteratee.error("")
    else newXmlParser(xml)
  }

  protected def parseStartTagName(input: Iterator[Int], xml: X): Unit = {
    val c = input.head
    if (isNameStartChar(c)) {
      input.step()
      xml.tagNameBegin()
      xml.tagNameAppend(c)
      xml.become(StartTagNameRest)
    }
    else xml.error("expected start tag name", input)
  }

  protected def parseStartTagNameRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isNameChar(c) }) { input.step(); xml.tagNameAppend(c) }
    if (!input.isEmpty) xml.become(StartTagRest)
    else if (input.isDone) xml.error("unexpected end of input", input)
  }

  protected def parseEndTagName(input: Iterator[Int], xml: X): Unit = {
    val c = input.head
    if (isNameStartChar(c)) {
      input.step()
      xml.tagNameBegin()
      xml.tagNameAppend(c)
      xml.become(EndTagNameRest)
    }
    else xml.error("expected end tag name", input)
  }

  protected def parseEndTagNameRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isNameChar(c) }) { input.step(); xml.tagNameAppend(c) }
    if (!input.isEmpty) xml.become(EndTagRest)
    else if (input.isDone) xml.error("unexpected end of input", input)
  }

  protected def parseAttrName(input: Iterator[Int], xml: X): Unit = {
    val c = input.head
    if (isNameStartChar(c)) {
      input.step()
      xml.attrNameBegin()
      xml.attrNameAppend(c)
      xml.become(AttrNameRest)
    }
    else xml.error("expected attribute name", input)
  }

  protected def parseAttrNameRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isNameChar(c) }) { input.step(); xml.attrNameAppend(c) }
    if (!input.isEmpty) xml.become(AttrRest)
    else if (input.isDone) xml.error("unexpected end of input", input)
  }

  protected def parseAttrRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
    if (!input.isEmpty) c match {
      case '=' => input.step(); xml.become(AttrValue)
      case _ => xml.error("expected '='", input)
    }
  }

  protected def parseAttrValue(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
    if (!input.isEmpty) c match {
      case '"' => input.step(); xml.become(AttrValueDQRest); xml.attrValueBegin()
      case '\'' => input.step(); xml.become(AttrValueSQRest); xml.attrValueBegin()
      case _ => xml.error("expected quoted attribute value", input)
    }
  }

  protected def parseAttrValueDQRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; c != '"' && c != '&' && c != '<' && isChar(c) }) {
      input.step()
      xml.attrValueAppend(c)
    }
    if (!input.isEmpty) c match {
      case '"' => input.step(); xml.attrValueEnd(); xml.attrEnd(); xml.become(StartTagRest)
      case _ => xml.error("unexpected character in attribute value", input)
    }
    else if (input.isDone) xml.error("unexpected end of input", input)
  }

  protected def parseAttrValueSQRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; c != '\'' && c != '&' && c != '<' && isChar(c) }) {
      input.step()
      xml.attrValueAppend(c)
    }
    if (!input.isEmpty) c match {
      case '\'' => input.step(); xml.attrValueEnd(); xml.attrEnd(); xml.become(StartTagRest)
      case _ => xml.error("unexpected character in attribute value", input)
    }
    else if (input.isDone) xml.error("unexpected end of input", input)
  }

  protected def parseStartTagRest(input: Iterator[Int], xml: X): Unit = input.head match {
    case '/' => input.step(); xml.become(EmptyTagRest)
    case '>' => input.step(); xml.openTagEnd(); xml.become(Content)
    case c =>
      if (isSpace(c)) { input.step(); xml.become(StartTagSpace) }
      else xml.error("expected space or tag close", input)
  }

  protected def parseStartTagSpace(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
    if (!input.isEmpty) c match {
      case '/' => input.step(); xml.become(EmptyTagRest)
      case '>' => input.step(); xml.openTagEnd(); xml.become(Content)
      case _ if isNameStartChar(c) => xml.become(AttrName); xml.attrBegin()
      case _ => xml.error("expected space, attribute, or tag close", input)
    }
  }

  protected def parseEmptyTagRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
    if (!input.isEmpty) c match {
      case '>' => input.step(); xml.openTagClose(); xml.become(Content)
      case _ => xml.error("expected space or tag close", input)
    }
  }

  protected def parseEndTagRest(input: Iterator[Int], xml: X): Unit = {
    var c = 0
    while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
    if (!input.isEmpty) c match {
      case '>' => input.step(); xml.closeTagEnd(); xml.become(Content)
      case _ => xml.error("expected space or tag close", input)
    }
  }

  protected def parseContent(input: Iterator[Int], xml: X): Unit = input.head match {
    case '&' => input.step(); xml.become(EntityRefRest)
    case '<' => input.step(); xml.become(AngleBracketRest)
    case _ => xml.become(CharData)
  }

  protected def parseAngleBracketRest(input: Iterator[Int], xml: X): Unit = input.head match {
    case '!' => input.step(); xml.become(CommentRest)
    case '/' => input.step(); xml.become(EndTagName); xml.closeTagBegin()
    case _ => xml.become(StartTagName); xml.openTagBegin()
  }
}

private[markup] object XmlParser {
  private final val ParseError       = -1
  private final val StartTagName     = 1
  private final val StartTagNameRest = 2
  private final val EndTagName       = 3
  private final val EndTagNameRest   = 4
  private final val AttrName         = 5
  private final val AttrNameRest     = 6
  private final val AttrRest         = 7
  private final val AttrValue        = 8
  private final val AttrValueDQRest  = 9
  private final val AttrValueSQRest  = 10
  private final val EntityRefRest    = 11
  private final val CharData         = 14
  private final val CommentRest      = 15
  private final val StartTagRest     = 40
  private final val StartTagSpace    = 41
  private final val EmptyTagRest     = 42
  private final val EndTagRest       = 43
  private final val Content          = 45
  private final val AngleBracketRest = 100
}
