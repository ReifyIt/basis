//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

class IndexTrieDataSpec
  extends FlatSpec
  with DataFactoryBehaviors
  with LoaderBehaviors
  with ReaderBehaviors
  with WriterBehaviors {

  override def suiteName = "IndexTrieData specification"

  "Big-endian index trie data" should behave like BigEndianLoader(IndexTrieDataBE)
  it should behave like ArrayLoader(IndexTrieDataBE)

  "Big-endian index trie data readers" should behave like BigEndianReader(IndexTrieDataBE)

  "Big-endian index trie data writers" should behave like BigEndianWriter(IndexTrieDataBE)


  "Little-endian index trie data" should behave like LittleEndianLoader(IndexTrieDataLE)
  it should behave like ArrayLoader(IndexTrieDataLE)

  "Little-endian index trie data readers" should behave like LittleEndianReader(IndexTrieDataLE)

  "Little-endian index trie data writers" should behave like LittleEndianWriter(IndexTrieDataLE)


  "Big-endian index trie data framers" should behave like DataSerializer(IndexTrieDataBE)

  "Little-endian index trie data framers" should behave like DataSerializer(IndexTrieDataLE)
}
