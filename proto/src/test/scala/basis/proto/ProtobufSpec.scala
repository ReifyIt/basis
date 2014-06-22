//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.proto

import basis.data._
import org.scalatest._

class ProtobufSpec extends FlatSpec with ProtobufBehaviors {
  override def suiteName = "Protobuf specification"

  "Big-endian protobuf array data" should behave like ProtobufTranscoder(ArrayDataBE)

  "Little-endian protobuf array data" should behave like ProtobufTranscoder(ArrayDataLE)

  "Big-endian protobuf index trie data" should behave like ProtobufTranscoder(IndexTrieDataBE)

  "Little-endian protobuf index trie data" should behave like ProtobufTranscoder(IndexTrieDataLE)

  "Big-endian protobuf finger trie data" should behave like ProtobufTranscoder(FingerTrieDataBE)

  "Little-endian protobuf finger trie data" should behave like ProtobufTranscoder(FingerTrieDataLE)
}
