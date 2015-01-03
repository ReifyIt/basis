//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.markup

import basis._

trait XmlBuilder extends State[Any] {
  def bindPrefix(prefix: String, namespace: String): this.type

  def appendAttr(prefix: String, name: String, value: String): this.type

  def appendAttr(name: String, value: String): this.type

  def appendElem(prefix: String, name: String): this.type

  def appendElem(name: String): this.type

  def beginElem(prefix: String, name: String): this.type

  def beginElem(name: String): this.type

  def endElem(): this.type

  def appendText(value: String): this.type

  def appendComment(value: String): this.type

  def appendEntityRef(name: String): this.type

  def state: State
}
