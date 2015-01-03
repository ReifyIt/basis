//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.markup

import basis._
import basis.collections._
import basis.text._
import basis.util._

private[markup] abstract class XmlParsee extends State[Any] {
  def accept: Int
  def become(state: Int): Unit

  def tagNameBegin(): Unit
  def tagNameAppend(c: Int): Unit
  def tagNameEnd(): Unit

  def attrNameBegin(): Unit
  def attrNameAppend(c: Int): Unit
  def attrNameEnd(): Unit

  def attrValueBegin(): Unit
  def attrValueAppend(c: Int): Unit
  def attrValueEnd(): Unit

  def attrBegin(): Unit
  def attrEnd(): Unit

  def openTagBegin(): Unit
  def openTagClose(): Unit
  def openTagEnd(): Unit

  def closeTagBegin(): Unit
  def closeTagEnd(): Unit

  def textBegin(): Unit
  def textAppend(c: Int): Unit
  def textEnd(): Unit

  def entityRefBegin(): Unit
  def entityRefAppend(c: Int): Unit
  def entityRefEnd(): Unit

  def commentBegin(): Unit
  def commentAppend(c: Int): Unit
  def commentEnd(): Unit

  def procInstBegin(): Unit
  def procInstAppend(c: Int): Unit
  def procInstEnd(): Unit

  def error(message: String, input: Iterator[Int]): Unit

  def state: State
}

private[markup] final class XmlModelParsee extends XmlParsee with State[Xml.Elem] {
  private[this] var state0: Int = 45
  private[this] var tagName: StringBuilder with basis.State[Xml.Name] = _
  private[this] var attrName: StringBuilder with basis.State[Xml.Name] = _
  private[this] var attrValue: StringBuilder with basis.State[String] = _
  private[this] var attrs: Builder[(Xml.Name, String)] with basis.State[Xml.Attrs] = _
  private[this] var elem: Xml.Elem = _

  override def accept: Int = state0
  override def become(state: Int): Unit = state0 = state

  override def tagNameBegin(): Unit = tagName = Xml.NameBuilder
  override def tagNameAppend(c: Int): Unit = tagName.append(c)
  override def tagNameEnd(): Unit = ()

  override def attrNameBegin(): Unit = attrName = Xml.NameBuilder
  override def attrNameAppend(c: Int): Unit = attrName.append(c)
  override def attrNameEnd(): Unit = ()

  override def attrValueBegin(): Unit = attrValue = String.Builder
  override def attrValueAppend(c: Int): Unit = attrValue.append(c)
  override def attrValueEnd(): Unit = {
    if (attrs eq null) attrs = Xml.AttrsBuilder
    attrs.append(attrName.state -> attrValue.state)
    attrName = null
    attrValue = null
  }

  override def attrBegin(): Unit = if (attrs eq null) attrs = Xml.AttrsBuilder
  override def attrEnd(): Unit = ()

  override def openTagBegin(): Unit = ()

  override def openTagClose(): Unit = {
    elem =
      if (attrs eq null) Xml.Elem.empty(tagName.state)
      else Xml.Elem.empty(tagName.state, attrs.state)
    tagName = null
    attrs = null
  }

  override def openTagEnd(): Unit = {
    // TODO
    elem =
      if (attrs eq null) Xml.Elem(tagName.state)()
      else Xml.Elem(tagName.state, attrs.state)()
    tagName = null
    attrs = null
  }

  override def closeTagBegin(): Unit = ()

  override def closeTagEnd(): Unit = {
    // TODO
    tagName = null
  }

  override def textBegin(): Unit = Predef.???
  override def textAppend(c: Int): Unit = Predef.???
  override def textEnd(): Unit = Predef.???

  override def entityRefBegin(): Unit = Predef.???
  override def entityRefAppend(c: Int): Unit = Predef.???
  override def entityRefEnd(): Unit = Predef.???

  override def commentBegin(): Unit = Predef.???
  override def commentAppend(c: Int): Unit = Predef.???
  override def commentEnd(): Unit = Predef.???

  override def procInstBegin(): Unit = Predef.???
  override def procInstAppend(c: Int): Unit = Predef.???
  override def procInstEnd(): Unit = Predef.???

  override def error(message: String, input: Iterator[Int]): Unit = {
    Predef.println(message)
    Predef.???
  }

  override def state: Xml.Elem = elem
}

private[markup] class XmlModelParser(override val xml: XmlModelParsee) extends XmlParser[XmlModelParsee](xml) {
  def this() = this(null)

  protected override def newXmlParsee(): XmlModelParsee = new XmlModelParsee()

  protected override def newXmlParser(xml: XmlModelParsee): XmlModelParser = new XmlModelParser(xml)
}

object XmlModelParser extends XmlModelParser()
