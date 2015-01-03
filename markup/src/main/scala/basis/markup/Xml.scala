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
import basis.util._
import scala.annotation._
import scala.reflect._
import scala.runtime._

/** XML document model.
  *
  * @contentDiagram hideNodes "basis.markup.Xml.XmlName" "basis.markup.Xml.XmlAttrs" "basis.markup.Xml.XmlScope" "basis.markup.Xml.XmlDocument" "basis.markup.Xml.XmlNode" "basis.markup.Xml.XmlElem" "basis.markup.Xml.XmlText" "basis.markup.Xml.XmlComment" "basis.markup.Xml.XmlEntityRef"
  */
trait Xml { Xml =>
  /** A namespaced name.
    * @template */
  type Name <: XmlName

  /** An attribute map from namespaced names to string values.
    * @template */
  type Attrs <: XmlAttrs

  /** A stacked map from namespace prefixes to namespace URLs.
    * @template */
  type Scope <: XmlScope

  /** An XML document.
    * @template */
  type Document <: XmlDocument

  /** An XML tree node.
    * @template */
  type Node <: XmlNode

  /** An XML element node.
    * @template */
  type Elem <: XmlElem with Node

  /** An XML text node.
    * @template */
  type Text <: XmlText with Node

  /** An XML comment node.
    * @template */
  type Comment <: XmlComment with Node

  /** An XML entity reference node.
    * @template */
  type EntityRef <: XmlEntityRef with Node

  val Name: XmlNameFactory
  val Attrs: XmlAttrsFactory
  val Scope: XmlScopeFactory
  val Document: XmlDocumentFactory
  val Node: XmlNodeFactory
  val Elem: XmlElemFactory
  val Text: XmlTextFactory
  val Comment: XmlCommentFactory
  val EntityRef: XmlEntityRefFactory

  def NameBuilder: StringBuilder with From[Name] with State[Name] = Name.Builder
  def ElemBuilder(tag: Name, attrs: Attrs = Attrs.empty, scope: Scope = Scope.empty): Builder[Node] with State[Elem] = Elem.Builder(tag, attrs, scope)

  implicit def AttrsBuilder: Builder[(Name, String)] with From[Attrs] with State[Attrs]   = Attrs.Builder
  implicit def ScopeBuilder: Builder[(String, String)] with From[Scope] with State[Scope] = Scope.Builder
  implicit def TextBuilder: StringBuilder with From[Text] with State[Text]                = Text.Builder
  implicit def CommentBuilder: StringBuilder with From[Comment] with State[Comment]       = Comment.Builder

  implicit def NameTag: ClassTag[Name]
  implicit def AttrsTag: ClassTag[Attrs]
  implicit def ScopeTag: ClassTag[Scope]
  implicit def DocumentTag: ClassTag[Document]
  implicit def NodeTag: ClassTag[Node]
  implicit def ElemTag: ClassTag[Elem]
  implicit def TextTag: ClassTag[Text]
  implicit def CommentTag: ClassTag[Comment]
  implicit def EntityRefTag: ClassTag[EntityRef]

  implicit lazy val StringToName: String => Name = new StringToName
  implicit lazy val StringToText: String => Text = new StringToText


  trait XmlName extends Equals { this: Name =>
    def namespace: String
    def name: String

    def writeXml(builder: StringBuilder, scope: Scope): Unit = {
      if (namespace.length > 0) scope.getPrefix(namespace).fold(throw new XmlException(s"Unbound namespace: $namespace")) { prefix =>
        if (prefix.length > 0) {
          builder.append(prefix)
          builder.append(':')
        }
        builder.append(name)
      }
      else builder.append(name)
    }

    def toXml(scope: Scope): String = {
      val builder = String.Builder
      writeXml(builder, scope)
      builder.state
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[XmlName]

    override def equals(other: Any): Boolean = other match {
      case that: XmlName => that.canEqual(this) && namespace.equals(that.namespace) && name.equals(that.name)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Name], namespace.hashCode), name.hashCode))
    }

    override def toString: String = {
      val s = String.Builder~"Name"~'('
      if (namespace.length > 0) s~>namespace~", "~>name else s~>name
      (s~')').state
    }
  }

  abstract class XmlNameFactory {
    def apply(namespace: String, name: String): Name
    def apply(name: String): Name
    def unapply(name: Name): Maybe[(String, String)] = Bind((name.namespace, name.name))
    def Builder: StringBuilder with State[Name] = new XmlNameBuilder(String.Builder)
    override def toString: String = (String.Builder~Xml.toString~'.'~"Name").state
  }

  private final class XmlNameBuilder(underlying: StringBuilder with State[String]) extends StringBuilder with State[Name] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: Name                    = Name(underlying.state)
    override def toString: String               = (String.Builder~Xml.toString~'.'~"Name"~'.'~"Builder").state
  }


  trait XmlAttrs extends Equals with Immutable with Family[Attrs] with Map[Name, String] { this: Attrs =>
    def :+ (attr: (Name, String)): Attrs
    def +: (attr: (Name, String)): Attrs
    def + (name: Name, value: String): Attrs
    def - (name: Name): Attrs
    def ++ (that: Attrs): Attrs
    def -- (that: Attrs): Attrs
    protected override def stringPrefix: String = "Attrs"
  }

  abstract class XmlAttrsFactory extends special.MapSource[Attrs, Name, String] {
    override def toString: String = (String.Builder~Xml.toString~'.'~"Attrs").state
  }


  trait XmlScope extends Equals with Immutable with Family[Scope] with Map[String, String] { this: Scope =>
    def hasParent: Boolean = ne(parent)
    def parent: Scope
    def hasNamespace(prefix: String): Boolean
    def getNamespace(prefix: String): Maybe[String]
    def hasPrefix(namespace: String): Boolean
    def getPrefix(namespace: String): Maybe[String]
    def + (prefix: String, namespace: String): Scope
    def - (prefix: String): Scope
    def ++ (that: Scope): Scope
    def -- (that: Scope): Scope
    def :: (that: Scope): Scope
    protected override def stringPrefix: String = "Scope"
  }

  abstract class XmlScopeFactory extends special.MapSource[Scope, String, String] {
    override def toString: String = (String.Builder~Xml.toString~'.'~"Scope").state
  }


  trait XmlDocument extends Equals { this: Document =>
    def root: Elem

    def writeXml(builder: StringBuilder): Unit = {
      builder.append('<')
      builder.append('?')
      builder.append("xml")
      builder.append(' ')
      builder.append("version")
      builder.append('=')
      builder.append('"')
      builder.append("1.0")
      builder.append('"')
      builder.append('?')
      builder.append('>')
      root.writeXml(builder, Scope.empty)
    }

    def toXml(scope: Scope): String = {
      val builder = String.Builder
      writeXml(builder)
      builder.state
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[XmlDocument]

    override def equals(other: Any): Boolean = other match {
      case that: XmlDocument => root.equals(that.root)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Document], root.hashCode))
    }

    override def toString: String = (String.Builder~"Document"~'('~>root~')').state
  }

  abstract class XmlDocumentFactory {
    def apply(root: Elem): Document
    override def toString: String = (String.Builder~Xml.toString~'.'~"Document").state
  }


  trait XmlNode { this: Node =>
    def isElem: Boolean      = false
    def isText: Boolean      = false
    def isComment: Boolean   = false
    def isEntityRef: Boolean = false

    def asElem: Elem           = throw new MatchError("not an Elem node")
    def asText: Text           = throw new MatchError("not a Text node")
    def asComment: Comment     = throw new MatchError("not a Comment node")
    def asEntityRef: EntityRef = throw new MatchError("not an EntityRef node")

    def writeXml(builder: StringBuilder, scope: Scope): Unit

    def toXml(scope: Scope): String = {
      val builder = String.Builder
      writeXml(builder, scope)
      builder.state
    }

    def toXml: String = toXml(Scope.empty)

    protected[Xml] def writeText(cs: Iterator[Int], builder: StringBuilder): Unit = {
      while (!cs.isEmpty) {
        (cs.head: @switch) match {
          case '&' => builder.append("&amp;")
          case '<' => builder.append("&lt;")
          case '>' => builder.append("&gt;")
          case '"' => builder.append("&quot;")
          case c   => builder.append(c)
        }
        cs.step()
      }
    }
  }

  abstract class XmlNodeFactory {
    override def toString: String = (String.Builder~Xml.toString~'.'~"Node").state
  }


  trait XmlElem extends Equals with Immutable with Family[Elem] with IndexedSeq[Node] with XmlNode { this: Elem =>
    override def isElem: Boolean = true
    override def asElem: Elem    = this

    def tag: Name
    def attrs: Attrs
    def scope: Scope

    def :+ (node: Node): Elem
    def +: (node: Node): Elem

    override def writeXml(builder: StringBuilder, scope: Scope): Unit = {
      val context = this.scope :: scope
      builder.append('<')
      tag.writeXml(builder, context)
      for (attr <- attrs) {
        builder.append(' ')
        attr._1.writeXml(builder, context)
        builder.append('=')
        builder.append('"')
        writeText(new UString(attr._2).iterator, builder)
        builder.append('"')
      }
      for (binding <- context) {
        // Don't reintroduce namespace bindings
        if (!context.hasParent || context.parent.getNamespace(binding._1).fold(true)(!_.equals(binding._2))) {
          builder.append(' ')
          builder.append("xmlns")
          if (binding._1.length > 0) {
            builder.append(':')
            builder.append(binding._1)
          }
          builder.append('=')
          builder.append('"')
          builder.append(binding._2)
          builder.append('"')
        }
      }
      if (!isEmpty) {
        builder.append('>')
        for (node <- this) node.writeXml(builder, context)
        builder.append('<')
        builder.append('/')
        tag.writeXml(builder, context)
        builder.append('>')
      }
      else {
        builder.append('/')
        builder.append('>')
      }
    }

    protected override def stringPrefix: String = {
      val s = String.Builder~"Elem"~'('
      if (tag.namespace.length > 0) s~>tag else s~>tag.name
      if (!attrs.isEmpty) {
        s~", "~>attrs
        if (!scope.isEmpty) s~", "~>scope
      }
      else if (!scope.isEmpty) s~", "~"scope"~" = "~>scope
      (s~')').state
    }
  }

  abstract class XmlElemFactory {
    def empty(tag: Name, attrs: Attrs = Attrs.empty, scope: Scope = Scope.empty): Elem
    def apply(tag: Name, attrs: Attrs = Attrs.empty, scope: Scope = Scope.empty)(children: Node*): Elem
    def Builder(tag: Name, attrs: Attrs = Attrs.empty, scope: Scope = Scope.empty): Builder[Node] with State[Elem]
    override def toString: String = (String.Builder~Xml.toString~'.'~"Elem").state
  }


  trait XmlText extends Equals with Family[Text] with UTF with XmlNode { this: Text =>
    override def isText: Boolean = true
    override def asText: Text    = this
    override def writeXml(builder: StringBuilder, scope: Scope): Unit = writeText(iterator, builder)
    protected override def stringPrefix: String = "Text"
  }

  abstract class XmlTextFactory extends StringFactory[Text] {
    override def toString: String = (String.Builder~Xml.toString~'.'~"Text").state
  }


  trait XmlComment extends Equals with Family[Comment] with UTF with XmlNode { this: Comment =>
    override def isComment: Boolean = true
    override def asComment: Comment = this
    override def writeXml(builder: StringBuilder, scope: Scope): Unit = writeText(iterator, builder)
    protected override def stringPrefix: String = "Comment"
  }

  abstract class XmlCommentFactory extends StringFactory[Comment] {
    override def toString: String = (String.Builder~Xml.toString~'.'~"Comment").state
  }


  trait XmlEntityRef extends Equals with XmlNode { this: EntityRef =>
    override def isEntityRef: Boolean   = true
    override def asEntityRef: EntityRef = this

    def name: String

    override def writeXml(builder: StringBuilder, scope: Scope): Unit = {
      builder.append('&')
      builder.append(name)
      builder.append(';')
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[XmlEntityRef]

    override def equals(other: Any): Boolean = other match {
      case that: XmlEntityRef => that.canEqual(this) && name.equals(that.name)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[EntityRef], name.hashCode))
    }

    override def toString: String = (String.Builder~"EntityRef"~'('~>name~')').state
  }

  abstract class XmlEntityRefFactory {
    def apply(name: String): EntityRef
    def unapply(entityRef: EntityRef): Maybe[String] = Bind(entityRef.name)
    override def toString: String = (String.Builder~Xml.toString~'.'~"EntityRef").state
  }


  private final class StringToName extends AbstractFunction1[String, Name] {
    override def apply(name: String): Name = Name(name)
    override def toString: String = (String.Builder~Xml.toString~'.'~"StringToName").state
  }

  private final class StringToText extends AbstractFunction1[String, Text] {
    override def apply(content: String): Text = Text(content)
    override def toString: String = (String.Builder~Xml.toString~'.'~"StringToText").state
  }
}

object Xml extends Xml {
  final class Name(override val namespace: String, override val name: String) extends XmlName

  object Name extends XmlNameFactory {
    override def apply(namespace: String, name: String): Name = new Name(namespace, name)
    override def apply(name: String): Name                    = new Name("", name)
  }


  final class Attrs(protected val underlying: FingerTrieSeq[(Name, String)]) extends XmlAttrs {
    private[this] var _index: HashTrieMap[Name, String] = null
    private[this] def index: HashTrieMap[Name, String] = {
      if (_index == null) _index = HashTrieMap.from(underlying)
      _index
    }

    override def isEmpty: Boolean = underlying.isEmpty

    override def size: Int = underlying.length

    override def contains(name: Name): Boolean =
      if (size > 8) index.contains(name)
      else (underlying: Container[(Name, String)]).exists(_._1.equals(name))

    override def get(name: Name): Maybe[String] =
      if (size > 8) index.get(name)
      else (underlying: Container[(Name, String)]).find(_._1.equals(name)).map(_._2)

    override def apply(name: Name): String =
      if (size > 8) index(name)
      else {
        val these = underlying.iterator
        while (!these.isEmpty) {
          val attr = these.head
          if (attr._1.equals(name)) return attr._2
          these.step()
        }
        throw new NoSuchElementException(name.toString)
      }

    override def :+ (attr: (Name, String)): Attrs = new Attrs(underlying :+ attr)

    override def +: (attr: (Name, String)): Attrs = new Attrs(attr +: underlying)

    override def + (name: Name, value: String): Attrs =
      if (_index != null && !_index.contains(name)) new Attrs(underlying :+ (name -> value))
      else {
        var i = size - 1
        while (i >= 0 && !underlying(i)._1.equals(name)) i -= 1
        if (i >= 0) {
          if (underlying(i)._2.equals(value)) this
          else new Attrs(underlying.update(i, name -> value))
        }
        else new Attrs(underlying :+ (name -> value))
      }

    override def - (name: Name): Attrs =
      if (_index != null && !_index.contains(name)) this
      else (underlying: Container[(Name, String)]).filter(!_._1.equals(name))(AttrsBuilder)

    override def ++ (that: Attrs): Attrs = (underlying: Container[(Name, String)]).++(that.underlying)(AttrsBuilder)

    override def -- (that: Attrs): Attrs = (underlying: Container[(Name, String)]).filter(attr => !that.contains(attr._1))(AttrsBuilder)

    override def iterator: Iterator[(Name, String)] = underlying.iterator

    override def traverse(f: ((Name, String)) => Unit): Unit = underlying.traverse(f)
  }

  object Attrs extends XmlAttrsFactory {
    override val empty: Attrs                                                 = new Attrs(FingerTrieSeq.empty)
    implicit override def Builder: Builder[(Name, String)] with State[Attrs] = new AttrsBuilder(FingerTrieSeq.Builder[(Name, String)])
  }

  private final class AttrsBuilder(underlying: Builder[(Name, String)] with State[FingerTrieSeq[(Name, String)]]) extends Builder[(Name, String)] with State[Attrs] {
    override def append(attr: (Name, String)): Unit = underlying.append(attr)
    override def clear(): Unit                      = underlying.clear()
    override def expect(count: Int): this.type      = { underlying.expect(count); this }
    override def state: Attrs                       = new Attrs(underlying.state)
    override def toString: String                   = (String.Builder~Xml.toString~'.'~"Attrs"~'.'~"Builder").state
  }


  final class Scope(_parent: Scope, protected val bindings: HashTrieMap[String, String], private var _mappings: HashTrieMap[String, String]) extends XmlScope {
    def this(bindings: HashTrieMap[String, String]) = this(null, bindings, null)

    override val parent: Scope = if (_parent != null) _parent else this

    protected def mappings: HashTrieMap[String, String] = {
      if (_mappings == null) _mappings = bindings.map(binding => binding._2 -> binding._1)(HashTrieMap.Builder)
      _mappings
    }

    override def hasNamespace(prefix: String): Boolean =
      bindings.contains(prefix) || ne(parent) && parent.hasNamespace(prefix)

    override def getNamespace(prefix: String): Maybe[String] = {
      val maybeNamespace = bindings.get(prefix)
      if (maybeNamespace.canBind || eq(parent)) maybeNamespace
      else parent.getNamespace(prefix)
    }

    override def hasPrefix(namespace: String): Boolean =
      mappings.contains(namespace) || ne(parent) && parent.hasPrefix(namespace)

    override def getPrefix(namespace: String): Maybe[String] = {
      val maybePrefix = mappings.get(namespace)
      if (maybePrefix.canBind || eq(parent)) maybePrefix
      else parent.getPrefix(namespace)
    }

    override def :: (that: Scope): Scope = {
      if      (this.isEmpty && that.eq(that.parent)) that
      else if (that.isEmpty && this.eq(this.parent)) this
      else new Scope(this, that.bindings, that._mappings)
    }

    override def isEmpty: Boolean                              = bindings.isEmpty
    override def size: Int                                     = bindings.size
    override def contains(prefix: String): Boolean             = bindings.contains(prefix)
    override def get(prefix: String): Maybe[String]            = bindings.get(prefix)
    override def apply(prefix: String): String                 = bindings(prefix)
    override def + (prefix: String, namespace: String): Scope  = new Scope(bindings + (prefix, namespace))
    override def - (prefix: String): Scope                     = new Scope(bindings - prefix)
    override def ++ (that: Scope): Scope                       = bindings.++(that.bindings)(ScopeBuilder)
    override def -- (that: Scope): Scope                       = bindings.filter(binding => !that.contains(binding._1))(ScopeBuilder)
    override def iterator: Iterator[(String, String)]          = bindings.iterator
    override def traverse(f: ((String, String)) => Unit): Unit = bindings.traverse(f)
  }

  object Scope extends XmlScopeFactory {
    override val empty: Scope                                                  = new Scope(HashTrieMap.empty)
    implicit override def Builder: Builder[(String, String)] with State[Scope] = new ScopeBuilder(HashTrieMap.Builder[String, String])
  }

  private final class ScopeBuilder(underlying: Builder[(String, String)] with State[HashTrieMap[String, String]]) extends Builder[(String, String)] with State[Scope] {
    override def append(binding: (String, String)): Unit = underlying.append(binding)
    override def clear(): Unit                           = underlying.clear()
    override def expect(count: Int): this.type           = { underlying.expect(count); this }
    override def state: Scope                            = new Scope(underlying.state)
    override def toString: String                        = (String.Builder~Xml.toString~'.'~"Scope"~'.'~"Builder").state
  }


  final class Document(override val root: Elem) extends XmlDocument

  object Document extends XmlDocumentFactory {
    override def apply(root: Elem): Document = new Document(root)
  }


  sealed abstract class Node extends XmlNode

  object Node extends XmlNodeFactory


  final class Elem(
      override val tag: Name,
      override val attrs: Attrs,
      override val scope: Scope,
      protected val children: FingerTrieSeq[Node])
    extends Node with XmlElem {
    override def isEmpty: Boolean                = children.isEmpty
    override def length: Int                     = children.length
    override def apply(index: Int): Node         = children(index)
    override def :+ (node: Node): Elem           = new Elem(tag, attrs, scope, children :+ node)
    override def +: (node: Node): Elem           = new Elem(tag, attrs, scope, node +: children)
    override def iterator: Iterator[Node]        = children.iterator
    override def traverse(f: Node => Unit): Unit = children.traverse(f)
  }

  object Elem extends XmlElemFactory {
    def empty(tag: Name, attrs: Attrs, scope: Scope): Elem                                      = new Elem(tag, attrs, scope, FingerTrieSeq.empty)
    def apply(tag: Name, attrs: Attrs, scope: Scope, children: FingerTrieSeq[Node]): Elem       = new Elem(tag, attrs, scope, children)
    override def apply(tag: Name, attrs: Attrs, scope: Scope)(children: Node*): Elem            = new Elem(tag, attrs, scope, FingerTrieSeq.from(children))
    override def Builder(tag: Name, attrs: Attrs, scope: Scope): Builder[Node] with State[Elem] = new ElemBuilder(tag, attrs, scope, FingerTrieSeq.Builder[Node])
  }

  private final class ElemBuilder(tag: Name, attrs: Attrs, scope: Scope, children: Builder[Node] with State[FingerTrieSeq[Node]]) extends Builder[Node] with State[Elem] {
    override def append(node: Node): Unit      = children.append(node)
    override def clear(): Unit                 = children.clear()
    override def expect(count: Int): this.type = { children.expect(count); this }
    override def state: Elem                   = new Elem(tag, attrs, scope, children.state)
    override def toString: String              = (String.Builder~Xml.toString~'.'~"Elem"~'.'~"Builder").state
  }


  final class Text(protected val underlying: UString) extends Node with XmlText {
    override def iterator: Iterator[Int] = underlying.iterator
    override def toUString: UString      = underlying
  }

  object Text extends XmlTextFactory {
    override val empty: Text                                      = new Text(new UString(""))
    implicit override def Builder: StringBuilder with State[Text] = new TextBuilder(UString.Builder)
  }

  private final class TextBuilder(underlying: StringBuilder with State[UString]) extends StringBuilder with State[Text] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: Text                    = new Text(underlying.state)
    override def toString: String               = (String.Builder~Xml.toString~'.'~"Text"~'.'~"Builder").state
  }


  final class Comment(protected val underlying: UString) extends Node with XmlComment {
    override def iterator: Iterator[Int] = underlying.iterator
    override def toUString: UString      = underlying
  }

  object Comment extends XmlCommentFactory {
    override val empty: Comment                                      = new Comment(new UString(""))
    implicit override def Builder: StringBuilder with State[Comment] = new CommentBuilder(UString.Builder)
  }

  private final class CommentBuilder(underlying: StringBuilder with State[UString]) extends StringBuilder with State[Comment] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: Comment                 = new Comment(underlying.state)
    override def toString: String               = (String.Builder~Xml.toString~'.'~"Comment"~'.'~"Builder").state
  }


  final class EntityRef(override val name: String) extends Node with XmlEntityRef

  object EntityRef extends XmlEntityRefFactory {
    override def apply(name: String): EntityRef = new EntityRef(name)
  }


  implicit override lazy val NameTag: ClassTag[Name]           = ClassTag(Predef.classOf[Name])
  implicit override lazy val AttrsTag: ClassTag[Attrs]         = ClassTag(Predef.classOf[Attrs])
  implicit override lazy val ScopeTag: ClassTag[Scope]         = ClassTag(Predef.classOf[Scope])
  implicit override lazy val DocumentTag: ClassTag[Document]   = ClassTag(Predef.classOf[Document])
  implicit override lazy val NodeTag: ClassTag[Node]           = ClassTag(Predef.classOf[Node])
  implicit override lazy val ElemTag: ClassTag[Elem]           = ClassTag(Predef.classOf[Elem])
  implicit override lazy val TextTag: ClassTag[Text]           = ClassTag(Predef.classOf[Text])
  implicit override lazy val CommentTag: ClassTag[Comment]     = ClassTag(Predef.classOf[Comment])
  implicit override lazy val EntityRefTag: ClassTag[EntityRef] = ClassTag(Predef.classOf[EntityRef])

  override def toString: String = "Xml"
}
