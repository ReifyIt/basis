//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.collections._
import basis.memory._
import basis.text._
import basis.util._
import scala.annotation._

trait BsonReader { variant: BsonVariant =>
  private[bson] final class BsonReader(override val underlying: Reader) extends ProxyReader {
    def readBsonDouble(): AnyForm = BsonDouble(readUnalignedDouble())

    def readBsonString(implicit builder: StringBuilder): builder.State = {
      val utf8Builder = UTF8.Builder(builder)
      var i = 0
      val n = readUnalignedInt() - 1
      while (i < n) {
        utf8Builder.append(readByte() & 0xFF)
        i += 1
      }
      if (readByte() != 0) throw new BsonException("unterminated string")
      utf8Builder.state
    }

    def readBsonObject(implicit builder: Builder[(String, AnyForm)]): builder.State = {
      readUnalignedInt() // size
      var tag = readByte()
      while (tag != 0) {
        (tag: @switch) match {
          case 0x01 => builder.append(readCString() -> readBsonDouble())
          case 0x02 => builder.append(readCString() -> BsonStringValue(readBsonString(BsonStringBuilder())))
          case 0x03 => builder.append(readCString() -> BsonObjectValue(readBsonObject(BsonObjectBuilder())))
          case 0x04 => builder.append(readCString() -> BsonArrayValue(readBsonArray(BsonArrayBuilder())))
          case 0x05 => builder.append(readCString() -> readBsonBinary())
          case 0x06 => builder.append(readCString() -> readBsonUndefined())
          case 0x07 => builder.append(readCString() -> readBsonObjectId())
          case 0x08 => builder.append(readCString() -> readBsonBoolean())
          case 0x09 => builder.append(readCString() -> readBsonDateTime())
          case 0x0A => builder.append(readCString() -> readBsonNull())
          case 0x0B => builder.append(readCString() -> readBsonRegex())
          case 0x0C => builder.append(readCString() -> readBsonDBPointer())
          case 0x0D => builder.append(readCString() -> readBsonJSCode())
          case 0x0E => builder.append(readCString() -> readBsonSymbol())
          case 0x0F => builder.append(readCString() -> readBsonJSScope())
          case 0x10 => builder.append(readCString() -> readBsonInt32())
          case 0x11 => builder.append(readCString() -> readBsonTimeStamp())
          case 0x12 => builder.append(readCString() -> readBsonInt64())
          case 0x7F => builder.append(readCString() -> readBsonMinKey())
          case 0xFF => builder.append(readCString() -> readBsonMaxKey())
          case tag  => throw new BsonException("unknown bson type: "+ tag)
        }
        tag = readByte()
      }
      builder.state
    }

    def readBsonArray(implicit builder: Builder[AnyForm]): builder.State = {
      readUnalignedInt() // size
      var tag = readByte()
      while (tag != 0) {
        (tag: @switch) match {
          case 0x01 => skipCString(); builder.append(readBsonDouble())
          case 0x02 => skipCString(); builder.append(BsonStringValue(readBsonString(BsonStringBuilder())))
          case 0x03 => skipCString(); builder.append(BsonObjectValue(readBsonObject(BsonObjectBuilder())))
          case 0x04 => skipCString(); builder.append(BsonArrayValue(readBsonArray(BsonArrayBuilder())))
          case 0x05 => skipCString(); builder.append(readBsonBinary())
          case 0x06 => skipCString(); builder.append(readBsonUndefined())
          case 0x07 => skipCString(); builder.append(readBsonObjectId())
          case 0x08 => skipCString(); builder.append(readBsonBoolean())
          case 0x09 => skipCString(); builder.append(readBsonDateTime())
          case 0x0A => skipCString(); builder.append(readBsonNull())
          case 0x0B => skipCString(); builder.append(readBsonRegex())
          case 0x0C => skipCString(); builder.append(readBsonDBPointer())
          case 0x0D => skipCString(); builder.append(readBsonJSCode())
          case 0x0E => skipCString(); builder.append(readBsonSymbol())
          case 0x0F => skipCString(); builder.append(readBsonJSScope())
          case 0x10 => skipCString(); builder.append(readBsonInt32())
          case 0x11 => skipCString(); builder.append(readBsonTimeStamp())
          case 0x12 => skipCString(); builder.append(readBsonInt64())
          case 0x7F => skipCString(); builder.append(readBsonMinKey())
          case 0xFF => skipCString(); builder.append(readBsonMaxKey())
          case tag  => throw new BsonException("unknown bson type: "+ tag)
        }
        tag = readByte()
      }
      builder.state
    }

    def readBsonBinary(): AnyForm = {
      val length = readUnalignedInt()
      val subtype = readByte()
      val data = readByteArray(length)
      BsonBinary(subtype, data)
    }

    def readBsonUndefined(): AnyForm = BsonUndefined

    def readBsonObjectId(): AnyForm = BsonObjectId(readByteArray(12))

    def readBsonBoolean(): AnyForm = BsonBoolean(readByte() != 0)

    def readBsonDateTime(): AnyForm = BsonDateTime(readUnalignedLong())

    def readBsonNull(): AnyForm = BsonNull

    def readBsonRegex(): AnyForm = {
      val pattern = readCString()
      val options = readCString()
      BsonRegex(pattern, options)
    }

    def readBsonDBPointer(): AnyForm = {
      val name = readUTF8String()
      val id = readByteArray(12)
      BsonDBPointer(name, id)
    }

    def readBsonJSCode(): AnyForm = {
      val js = readUTF8String()
      BsonJSCode(js)
    }

    def readBsonSymbol(): AnyForm = {
      val symbol = readUTF8String()
      BsonSymbol(symbol)
    }

    def readBsonJSScope(): AnyForm = {
      readUnalignedInt() // length
      val js = readUTF8String()
      val scope = readBsonObject(ObjectForm.Builder())
      BsonJSScope(js, scope)
    }

    def readBsonInt32(): AnyForm = BsonInt32(readUnalignedInt())

    def readBsonTimeStamp(): AnyForm = BsonTimeStamp(readUnalignedLong())

    def readBsonInt64(): AnyForm = BsonInt64(readUnalignedLong())

    def readBsonMinKey(): AnyForm = BsonMinKey

    def readBsonMaxKey(): AnyForm = BsonMaxKey

    def readUTF8String(): String = {
      var i = 0
      val n = readUnalignedInt() - 1
      val builder = UTF8.Builder(UString.Builder())
      while (i < n) {
        builder.append(readByte() & 0xFF)
        i += 1
      }
      if (readByte() != 0) throw new BsonException("unterminated string")
      builder.state.toString
    }

    def readCString(): String = {
      val builder = UTF8.Builder(UString.Builder())
      var b = readByte()
      while (b != 0) {
        builder.append(b & 0xFF)
        b = readByte()
      }
      builder.state.toString
    }

    def skipCString(): Unit = while (readByte() != 0) ()

    def readByteArray(length: Int): Array[Byte] = {
      var i = 0
      val array = new Array[Byte](length)
      while (i < length) {
        array(i) = readByte()
        i += 1
      }
      array
    }
  }
}
