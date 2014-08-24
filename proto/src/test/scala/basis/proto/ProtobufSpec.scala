//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.proto

import basis.collections.immutable._
import basis.data._
import basis.util._
import org.scalatest._

class ProtobufSpec extends FlatSpec with ProtobufBehaviors {
  override def suiteName = "Protobuf specification"

  "Big-endian protobuf array data" should behave like ProtobufTranscoder(ArrayDataBE)

  "Little-endian protobuf array data" should behave like ProtobufTranscoder(ArrayDataLE)

  "Big-endian protobuf index trie data" should behave like ProtobufTranscoder(IndexTrieDataBE)

  "Little-endian protobuf index trie data" should behave like ProtobufTranscoder(IndexTrieDataLE)

  "Big-endian protobuf finger trie data" should behave like ProtobufTranscoder(FingerTrieDataBE)

  "Little-endian protobuf finger trie data" should behave like ProtobufTranscoder(FingerTrieDataLE)

  "Protobuf messages" should "aggregate declared fields" in {
    object TestMessage extends Protobuf.Message[Nothing] {
      val Field1 = Protobuf.Required(1)(Protobuf.Varint)
      val Field2 = Protobuf.Required(2)(Protobuf.String)
      val fields: HashTrieMap[Long, Protobuf.Field[_]] = aggregateFields
      override def read(data: Reader): Nothing = throw new RuntimeException
      override def write(data: Writer, value: Nothing): Unit = ()
      override def sizeOf(value: Nothing): Int = 0
      override def wireType: Int = Protobuf.WireType.Message
    }
    val fields = HashTrieMap(
      TestMessage.Field1.key -> TestMessage.Field1,
      TestMessage.Field2.key -> TestMessage.Field2)
    TestMessage.fields should equal (fields)
  }
}
