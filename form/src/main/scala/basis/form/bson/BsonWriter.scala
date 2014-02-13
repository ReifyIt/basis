//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._
import basis.text._

private[bson] final class BsonWriter(val __ : Writer) extends AnyVal {
  def writeCString(string: UTF): Unit = {
    val cs = string.modifiedUTF8Iterator
    while (!cs.isEmpty) {
      __.writeByte(cs.head.toByte) // modified UTF-8 code units
      cs.step()
    }
    __.writeByte(0) // cstring sentinel
  }

  def writeCString(string: String): Unit = {
    val cs = new UString(string).modifiedUTF8Iterator
    while (!cs.isEmpty) {
      __.writeByte(cs.head.toByte) // modified UTF-8 code units
      cs.step()
    }
    __.writeByte(0) // cstring sentinel
  }
}
