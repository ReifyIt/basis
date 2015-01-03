//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.markup

import basis._
import basis.collections._
import basis.collections.immutable._
import basis.text._

private[markup] trait XmlMarkup {
  type XmlName

  type XmlAttr
  type XmlAttrs

  type XmlContent
  type XmlTag      <: XmlContent
  type XmlEmptyTag <: XmlTag
  type XmlStartTag <: XmlTag
  type XmlEndTag   <: XmlTag
  type XmlText     <: XmlContent
  type XmlComment  <: XmlContent

  def XmlNameBuilder: StringBuilder with State[XmlName]

  def XmlAttr(name: XmlName, value: String): XmlAttr

  def XmlAttrs(attrs: XmlAttr*): XmlAttrs
  def XmlAttrsBuilder: Builder[XmlAttr] with State[XmlAttrs]
  def XmlEmptyAttrs: XmlAttrs

  def XmlEmptyTag(name: XmlName, attrs: XmlAttrs): XmlEmptyTag
  def XmlEmptyTag(name: XmlName)(attrs: XmlAttr*): XmlEmptyTag
  def XmlEmptyTag(name: XmlName): XmlEmptyTag

  def XmlStartTag(name: XmlName, attrs: XmlAttrs): XmlStartTag
  def XmlStartTag(name: XmlName)(attrs: XmlAttr*): XmlStartTag
  def XmlStartTag(name: XmlName): XmlStartTag

  def XmlEndTag(name: XmlName): XmlEndTag

  def XmlTextBuilder: StringBuilder with State[XmlText]
  def XmlEmptyText: XmlText

  def XmlCommentBuilder: StringBuilder with State[XmlComment]
}

private[markup] object XmlMarkup extends XmlMarkup {
  override type XmlName = String

  override type XmlAttr = (XmlName, String)
  override type XmlAttrs = Seq[XmlAttr]

  override type XmlContent = AnyRef
  override type XmlText    = String

  override def XmlNameBuilder: StringBuilder with State[XmlName] = String.Builder

  override def XmlAttr(name: XmlName, value: String): XmlAttr = (name, value)

  override def XmlAttrs(attrs: XmlAttr*): XmlAttrs = FingerTrieSeq.from(attrs)
  override def XmlAttrsBuilder: Builder[XmlAttr] with State[XmlAttrs] = FingerTrieSeq.Builder
  override def XmlEmptyAttrs: XmlAttrs = FingerTrieSeq.empty

  override def XmlEmptyTag(name: XmlName, attrs: XmlAttrs): XmlEmptyTag = new XmlEmptyTag(name, attrs)
  override def XmlEmptyTag(name: XmlName)(attrs: XmlAttr*): XmlEmptyTag = new XmlEmptyTag(name, XmlAttrs(attrs: _*))
  override def XmlEmptyTag(name: XmlName): XmlEmptyTag = new XmlEmptyTag(name)

  override def XmlStartTag(name: XmlName, attrs: XmlAttrs): XmlStartTag = new XmlStartTag(name, attrs)
  override def XmlStartTag(name: XmlName)(attrs: XmlAttr*): XmlStartTag = new XmlStartTag(name, XmlAttrs(attrs: _*))
  override def XmlStartTag(name: XmlName): XmlStartTag = new XmlStartTag(name)

  override def XmlEndTag(name: XmlName): XmlEndTag = new XmlEndTag(name)

  override def XmlTextBuilder: StringBuilder with State[XmlText] = String.Builder
  override def XmlEmptyText: XmlText = ""

  override def XmlCommentBuilder: StringBuilder with State[XmlComment] = new XmlCommentBuilder(String.Builder)


  sealed abstract class XmlTag {
    def name: XmlName
    def attrs: XmlAttrs

    protected def showAttrs(s: StringBuilder): Unit = {
      val iter = attrs.iterator
      if (!iter.isEmpty) {
        val attr = iter.head
        s~>attr._1~" -> "~>attr._2
        iter.step()
        while (!iter.isEmpty) {
          val attr = iter.head
          s~", "~>attr._1~" -> "~>attr._2
          iter.step()
        }
      }
    }
  }


  final class XmlEmptyTag(override val name: XmlName, override val attrs: XmlAttrs) extends XmlTag {
    def this(name: XmlName) = this(name, FingerTrieSeq.empty)

    override def toString: String = {
      val s = String.Builder~"XmlEmptyTag"~'('~>name~')'
      if (!attrs.isEmpty) { s.append('('); showAttrs(s); s.append(')') }
      s.state
    }
  }


  final class XmlStartTag(override val name: XmlName, override val attrs: XmlAttrs) extends XmlTag {
    def this(name: XmlName) = this(name, FingerTrieSeq.empty)

    override def toString: String = {
      val s = String.Builder~"XmlStartTag"~'('~>name~')'
      if (!attrs.isEmpty) { s.append('('); showAttrs(s); s.append(')') }
      s.state
    }
  }


  final class XmlEndTag(override val name: XmlName) extends XmlTag {
    override val attrs: XmlAttrs = FingerTrieSeq.empty

    override def toString: String = (String.Builder~"XmlEndTag"~'('~>name~')').state
  }


  final class XmlComment(val content: String) {
    override def toString: String = (String.Builder~"XmlComment"~'('~>content~')').state
  }

  private final class XmlCommentBuilder(underlying: StringBuilder with State[String]) extends StringBuilder with State[XmlComment] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: XmlComment              = new XmlComment(underlying.state)
    override def toString: String               = (String.Builder~"XmlComment"~'.'~"Builder").state
  }
}
