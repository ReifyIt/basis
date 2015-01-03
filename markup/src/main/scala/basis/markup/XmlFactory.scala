//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.markup

import basis._
import basis.collections._
import basis.text._

private[markup] trait XmlFactory {
  type XmlName

  type XmlAttr
  type XmlAttrs

  type XmlNode
  type XmlElem <: XmlNode

  def XmlNameBuilder: StringBuilder with State[XmlName]

  def XmlAttr(name: XmlName, value: String): XmlAttr

  def XmlAttrsBuilder: Builder[XmlAttr] with State[XmlAttrs]

  def XmlElem(name: XmlName, attrs: XmlAttrs): XmlElem

  def XmlElemBuilder(name: XmlName, attrs: XmlAttrs): Builder[XmlNode] with State[XmlElem]
}
