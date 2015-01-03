//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.markup

import basis._
import basis.collections._
import basis.text._
import scala.annotation._

/* private[markup] */ class XmlParserOld[X <: XmlMarkup](val xml: X) {
  import xml._

  private def isSpace(c: Int): Boolean =
    c == 0x20 || c == 0x09 || c == 0x0D || c == 0x0A

  private def isChar(c: Int): Boolean =
    c == 0x9 || c == 0xA || c == 0xD ||
    c >= 0x20 && c <= 0xD7FF ||
    c >= 0xE000 && c <= 0xFFFD ||
    c >= 0x10000 && c <= 0x10FFFF

  private def isText(c: Int): Boolean =
    c != '&' && c != '<' && isChar(c)

  private def isNameStartChar(c: Int): Boolean =
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

  private def isNameChar(c: Int): Boolean =
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

  //private val Space: Iteratee[Int, Unit] = new Space()

  private val Spaces: Iteratee[Int, Unit] = new Spaces()

  val Name: Iteratee[Int, XmlName] = new Name()
  private def NameRest(c: Int): Iteratee[Int, XmlName] = new NameRest(c)

  val Tag: Iteratee[Int, XmlTag] = new Tag()
  private val TagRest: Iteratee[Int, XmlTag] = new TagRest()
  private def EmptyTagRest(name: XmlName, attrs: XmlAttrs): Iteratee[Int, XmlTag] = new EmptyTagRest(name, attrs)
  private def EmptyTagRest(name: XmlName): Iteratee[Int, XmlTag] = new EmptyTagRest(name)
  private val StartTagName: Iteratee[Int, XmlTag] = new StartTagName()
  private def StartTagNameRest(name: XmlName): Iteratee[Int, XmlTag] = new StartTagNameRest(name)
  private def StartTagNameSpace(name: XmlName): Iteratee[Int, XmlTag] = new StartTagNameSpace(name)
  private def StartTagAttrs(name: XmlName): Iteratee[Int, XmlTag] = new StartTagAttrs(name)
  private def StartTagAttrsRest(name: XmlName, attrs: XmlAttrs): Iteratee[Int, XmlTag] = new StartTagAttrsRest(name, attrs)
  private val EndTagName: Iteratee[Int, XmlTag] = new EndTagName()
  private def EndTagNameRest(name: XmlName): Iteratee[Int, XmlTag] = new EndTagNameRest(name)

  private val Eq: Iteratee[Int, Unit] = Spaces ~> '=' ~> Spaces
  private val AttrValue: Iteratee[Int, String] = new AttrValue()
  private val DoubleQuotedAttrValue: Iteratee[Int, String] = new DoubleQuotedAttrValue()
  private val SingleQuotedAttrValue: Iteratee[Int, String] = new SingleQuotedAttrValue()

  private val Attr: Iteratee[Int, XmlAttr] = new Attr()
  private def AttrSep(attrs: Builder[XmlAttr] with State[XmlAttrs]): Iteratee[Int, XmlAttrs] = new AttrSep(attrs)
  private def AttrSpace(attrs: Builder[XmlAttr] with State[XmlAttrs]): Iteratee[Int, XmlAttrs] = new AttrSpace(attrs)

  private def Attrs: Iteratee[Int, XmlAttrs] = new Attrs()
  private def Attrs(attrs: Builder[XmlAttr] with State[XmlAttrs]): Iteratee[Int, XmlAttrs] = new Attrs(attrs)

  val Text: Iteratee[Int, XmlText] = new Text()

  private val CommentRest: Iteratee[Int, XmlComment] = new CommentRest()
  val Comment: Iteratee[Int, XmlComment] = '<' ~> '!' ~> CommentRest

  private val BracketRest: Iteratee[Int, XmlContent] = new BracketRest()
  val Content: Iteratee[Int, XmlContent] = new Content()
  def Contents: Iteratee[Int, Seq[XmlContent]] = new Contents()

  implicit private def ExpectChar(e: Int): Iteratee[Int, Int] = new ExpectChar(e)

  private def describe(c: Int): String = (String.Builder~'\''~c~'\'').state


  //private final class Space extends Iteratee.Step[Int, Unit] {
  //  override def step(c: Int): Iteratee[Int, Unit] =
  //    if (isSpace(c)) Spaces else error(describe(c))
  //  override def expected: String = "space"
  //}

  private final class Spaces extends Iteratee[Int, Unit] {
    override def feed(input: Iterator[Int]): Iteratee[Int, Unit] = {
      while (!input.isEmpty && isSpace(input.head)) input.step()
      if (!input.isEmpty || input.isDone) Iteratee.done(())
      else this
    }
  }

  private final class ExpectChar(e: Int) extends Iteratee.Step[Int, Int] {
    override def step(c: Int): Iteratee[Int, Int] =
      if (c == e) done(e) else error(describe(c))
    override def expected: String = describe(e)
  }

  private final class Name extends Iteratee.Step[Int, XmlName] {
    override def step(c: Int): Iteratee[Int, XmlName] =
      if (isNameStartChar(c)) NameRest(c) else error(describe(c))
    override def expected: String = "name"
  }

  private final class NameRest(s: StringBuilder with State[XmlName]) extends Iteratee[Int, XmlName] {
    def this(c: Int) = this(XmlNameBuilder~c)
    override def feed(input: Iterator[Int]): Iteratee[Int, XmlName] = {
      var c = 0
      while (!input.isEmpty && { c = input.head; isNameChar(c) }) { input.step(); s.append(c) }
      if (!input.isEmpty) Iteratee.done(s.state)
      else if (input.isDone) Iteratee.error("unexpected end of input")
      else this
    }
  }


  //
  // Tags
  //

  private final class Tag extends Iteratee.Step[Int, XmlTag] {
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (c == '<') TagRest else error(describe(c))
    override def expected: String = "tag"
  }

  private final class TagRest extends Iteratee.Step[Int, XmlTag] {
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (c == '/') EndTagName else StartTagName
    override def accept(c: Int, next: Iteratee[Int, XmlTag]): Boolean =
      !next.isError && c == '/'
  }

  private final class EmptyTagRest(name: XmlName, attrs: XmlAttrs) extends Iteratee.Step[Int, XmlTag] {
    def this(name: XmlName) = this(name, XmlEmptyAttrs)
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (isSpace(c)) this
      else if (c == '>') done(XmlEmptyTag(name, attrs))
      else error(describe(c))
    override def expected: String = "end of empty tag"
  }

  private final class StartTagName(self: Iteratee[Int, XmlName]) extends Iteratee.AndThen[Int, XmlName, XmlTag](self) {
    def this() = this(Name)
    override def done(name: XmlName): Iteratee[Int, XmlTag] = StartTagNameRest(name)
    override def cont(next: Iteratee[Int, XmlName]): Iteratee[Int, XmlTag] = new StartTagName(next)
  }

  private final class StartTagNameRest(name: XmlName) extends Iteratee.Step[Int, XmlTag] {
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (isSpace(c)) StartTagNameSpace(name)
      else if (c == '>') done(XmlStartTag(name))
      else if (c == '/') EmptyTagRest(name)
      else error(describe(c))
    override def expected: String = "space or close of start tag"
  }

  private final class StartTagNameSpace(name: XmlName) extends Iteratee.Step[Int, XmlTag] {
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (isSpace(c)) this
      else if (c == '>') done(XmlStartTag(name))
      else if (c == '/') EmptyTagRest(name)
      else if (isNameStartChar(c)) StartTagAttrs(name)
      else error(describe(c))
    override def accept(c: Int, next: Iteratee[Int, XmlTag]): Boolean =
      !next.isError && !next.isInstanceOf[StartTagAttrs]
    override def expected: String = "space, attribute, or end of start tag"
  }

  private final class StartTagAttrs(name: XmlName, attrs: Iteratee[Int, XmlAttrs])
    extends Iteratee.AndThen[Int, XmlAttrs, XmlTag](attrs) {
    def this(name: XmlName) = this(name, Attrs)
    override def done(attrs: XmlAttrs): Iteratee[Int, XmlTag] = StartTagAttrsRest(name, attrs)
    override def cont(next: Iteratee[Int, XmlAttrs]): Iteratee[Int, XmlTag] = new StartTagAttrs(name, next)
  }

  private final class StartTagAttrsRest(name: XmlName, attrs: XmlAttrs) extends Iteratee.Step[Int, XmlTag] {
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (c == '>') done(XmlStartTag(name, attrs))
      else if (c == '/') EmptyTagRest(name, attrs)
      else error(describe(c))
    override def expected: String = "end of start tag"
  }

  private final class EndTagName(self: Iteratee[Int, XmlName]) extends Iteratee.AndThen[Int, XmlName, XmlTag](self) {
    def this() = this(Name)
    override def done(name: XmlName): Iteratee[Int, XmlTag] = EndTagNameRest(name)
    override def cont(next: Iteratee[Int, XmlName]): Iteratee[Int, XmlTag] = new EndTagName(next)
  }

  private final class EndTagNameRest(name: XmlName) extends Iteratee.Step[Int, XmlTag] {
    override def step(c: Int): Iteratee[Int, XmlTag] =
      if (isSpace(c)) this
      else if (c == '>') done(XmlEndTag(name))
      else error(describe(c))
    override def expected: String = "space or close of end tag"
  }


  //
  // Attributes
  //

  private final class AttrValue extends Iteratee.Step[Int, String] {
    override def step(c: Int): Iteratee[Int, String] =
      if (c == '"') DoubleQuotedAttrValue
      else if (c == '\'') SingleQuotedAttrValue
      else error(describe(c))
    override def expected: String = "attribute value"
  }

  private final class DoubleQuotedAttrValue(s: StringBuilder with State[String]) extends Iteratee[Int, String] {
    def this() = this(null)
    override def feed(input: Iterator[Int]): Iteratee[Int, String] = {
      val s = if (this.s ne null) this.s else String.Builder
      var c = 0
      while (!input.isEmpty && { c = input.head; c != '"' && c != '&' && c != '<' && isChar(c) }) {
        input.step()
        s.append(c)
      }
      if (!input.isEmpty && input.head == '"') {
        input.step()
        Iteratee.done(s.state)
      }
      else if (!input.isEmpty || input.isDone) Iteratee.done(s.state)
      else new DoubleQuotedAttrValue(s)
    }
  }

  private final class SingleQuotedAttrValue(s: StringBuilder with State[String]) extends Iteratee[Int, String] {
    def this() = this(null)
    override def feed(input: Iterator[Int]): Iteratee[Int, String] = {
      val s = if (this.s ne null) this.s else String.Builder
      var c = 0
      while (!input.isEmpty && { c = input.head; c != '&' && c != '\'' && c != '<' && isChar(c) }) {
        input.step()
        s.append(c)
      }
      if (!input.isEmpty && input.head == '\'') {
        input.step()
        Iteratee.done(s.state)
      }
      else if (!input.isEmpty || input.isDone) Iteratee.done(s.state)
      else new SingleQuotedAttrValue(s)
    }
  }

  private final class Attr(self: Iteratee[Int, (XmlName, String)])
    extends Iteratee.AndThen[Int, (XmlName, String), XmlAttr](self) {
    def this() = this(Name <~ Eq <~> AttrValue)
    override def done(pair: (XmlName, String)): Iteratee[Int, XmlAttr] = Iteratee.done(XmlAttr(pair._1, pair._2))
    override def cont(next: Iteratee[Int, (XmlName, String)]): Iteratee[Int, XmlAttr] = new Attr(next)
  }

  private final class AttrSep(attrs: Builder[XmlAttr] with State[XmlAttrs]) extends Iteratee.Step[Int, XmlAttrs] {
    override def step(c: Int): Iteratee[Int, XmlAttrs] =
      if (isSpace(c)) AttrSpace(attrs) else done(attrs.state)
    override def accept(c: Int, next: Iteratee[Int, XmlAttrs]): Boolean = isSpace(c)
  }

  private final class AttrSpace(attrs: Builder[XmlAttr] with State[XmlAttrs]) extends Iteratee.Step[Int, XmlAttrs] {
    override def step(c: Int): Iteratee[Int, XmlAttrs] =
      if (isSpace(c)) this
      else if (isNameStartChar(c)) Attrs(attrs)
      else done(attrs.state)
    override def accept(c: Int, next: Iteratee[Int, XmlAttrs]): Boolean = isSpace(c)
  }

  private final class Attrs(
      attr: Iteratee[Int, XmlAttr],
      attrs: Builder[XmlAttr] with State[XmlAttrs])
    extends Iteratee.AndThen[Int, XmlAttr, XmlAttrs](attr) {
    def this(attrs: Builder[XmlAttr] with State[XmlAttrs]) = this(Attr, attrs)
    def this() = this(Attr, XmlAttrsBuilder)
    override def done(attr: XmlAttr): Iteratee[Int, XmlAttrs] = AttrSep(attrs += attr)
    override def cont(next: Iteratee[Int, XmlAttr]): Iteratee[Int, XmlAttrs] = new Attrs(next, attrs)
  }


  //
  // Text
  //

  private final class Text(s: StringBuilder with State[XmlText]) extends Iteratee[Int, XmlText] {
    def this() = this(null)
    override def feed(input: Iterator[Int]): Iteratee[Int, XmlText] = {
      val s = if (this.s ne null) this.s else XmlTextBuilder
      var c = 0
      while (!input.isEmpty && { c = input.head; isText(c) }) {
        input.step()
        s.append(c)
      }
      if (!input.isEmpty || input.isDone) Iteratee.done(s.state)
      else new Text(s)
    }
  }


  //
  // Comments
  //

  private final class CommentRest(s: StringBuilder with State[XmlComment], k: Int) extends Iteratee[Int, XmlComment] {
    def this() = this(null, 1)
    override def feed(input: Iterator[Int]): Iteratee[Int, XmlComment] = {
      val s = if (this.s ne null) this.s else XmlCommentBuilder
      var k = this.k
      while (!input.isEmpty && k > 0) k = (k: @switch) match {
        case 1 => parse1(input)
        case 2 => parse2(input)
        case 3 => parse3(input, s)
        case 4 => parse4(input, s)
        case 5 => parse5(input)
      }
      if (k == -2) Iteratee.done(s.state)
      else if (k == -1 && !input.isEmpty || input.isDone) Iteratee.error("malformed comment")
      else new CommentRest(s, k)
    }
    private def parse1(input: Iterator[Int]): Int =
      if (input.head == '-') { input.step(); 2 } else -1
    private def parse2(input: Iterator[Int]): Int =
      if (input.head == '-') { input.step(); 3 } else -1
    private def parse3(input: Iterator[Int], s: StringBuilder): Int = {
      var c = 0
      while (!input.isEmpty && { c = input.head; c != '-' && isChar(c) }) {
        input.step()
        s.append(c)
      }
      if (!input.isEmpty && c == '-') { input.step(); 4 } else -1
    }
    private def parse4(input: Iterator[Int], s: StringBuilder): Int =
      if (input.head == '-') { input.step(); 5 } else { s.append('-'); 3 }
    private def parse5(input: Iterator[Int]): Int =
      if (input.head == '>') { input.step(); -2 } else -1
  }


  //
  // Content
  //

  private final class BracketRest extends Iteratee.Step[Int, XmlContent] {
    override def step(c: Int): Iteratee[Int, XmlContent] =
      if (c == '!') CommentRest
      else if (c == '/') EndTagName
      else StartTagName
    override def accept(c: Int, next: Iteratee[Int, XmlContent]): Boolean =
      !next.isError && c == '!' || c == '/'
  }

  private final class Content extends Iteratee[Int, XmlContent] {
    override def feed(input: Iterator[Int]): Iteratee[Int, XmlContent] = {
      if (!input.isEmpty) {
        val c = input.head
        if (c == '&') { input.step(); Predef.??? } // EntityRefRest
        else if (c == '<') { input.step(); BracketRest }
        else Text
      }
      else if (!input.isDone) this
      else Iteratee.done(XmlEmptyText)
    }
  }

  private final class Contents(
      private[this] var content: Iteratee[Int, XmlContent],
      private[this] val contents: Builder[XmlContent] with State[Seq[XmlContent]])
    extends Iteratee[Int, Seq[XmlContent]] {
    def this() = this(Content, Seq.Builder)
    override def feed(input: Iterator[Int]): Iteratee[Int, Seq[XmlContent]] = {
      val next = content.feed(input)
      if (next.isDone) {
        if (input.isDone) Iteratee.done(contents.state)
        else {
          content = Content
          contents.append(next.bind)
          this
        }
      }
      else if (!next.isError) {
        content = next
        this
      }
      else next.asError
    }
  }
}

/* private[markup] */ object XmlParserOld extends XmlParserOld[XmlMarkup.type](XmlMarkup) {
  import XmlMarkup._

  val Elem: Iteratee[XmlContent, Xml.Elem] = new Elem()

  private final class Elem extends Iteratee.Step[XmlContent, Xml.Elem] {
    override def step(content: XmlContent): Iteratee[XmlContent, Xml.Elem] = content match {
      case tag: XmlEmptyTag => done(Xml.Elem.empty(Xml.Name(tag.name), tag.attrs.map(attr => (Xml.Name(attr._1), attr._2))(Xml.AttrsBuilder)))
      case _ => error(content.toString)
    }
    override def expected: String = "element"
  }
}
