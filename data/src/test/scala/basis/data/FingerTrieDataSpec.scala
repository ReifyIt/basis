//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

class FingerTrieDataSpec
  extends FlatSpec
  with DataFactoryBehaviors
  with LoaderBehaviors
  with ReaderBehaviors
  with WriterBehaviors
  with ProtobufBehaviors {

  override def suiteName = "FingerTrieData specification"

  "Big-endian finger trie data" should behave like BigEndianLoader(FingerTrieDataBE)
  it should behave like ProtobufTranscoder(FingerTrieDataBE)

  "Big-endian finger trie data readers" should behave like BigEndianReader(FingerTrieDataBE)

  "Big-endian finger trie data writers" should behave like BigEndianWriter(FingerTrieDataBE)


  "Little-endian finger trie data" should behave like LittleEndianLoader(FingerTrieDataLE)
  it should behave like ProtobufTranscoder(FingerTrieDataLE)

  "Little-endian finger trie data readers" should behave like LittleEndianReader(FingerTrieDataLE)

  "Little-endian finger trie data writers" should behave like LittleEndianWriter(FingerTrieDataLE)


  "Big-endian finger trie data framers" should behave like DataSerializer(FingerTrieDataBE)

  "Little-endian finger trie data framers" should behave like DataSerializer(FingerTrieDataLE)
}
