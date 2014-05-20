//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

class ByteArraySpec extends FlatSpec with ByteFactoryBehaviors with LoaderBehaviors with StorerBehaviors with ReaderBehaviors with WriterBehaviors {
  override def suiteName = "ByteArray specification"

  "Big-endian byte arrays" should behave like PrimitiveSerializer(ByteArrayBE)
  it should behave like BigEndianLoader(ByteArrayBE)
  it should behave like BigEndianStorer(ByteArrayBE)

  "Big-endian byte array readers" should behave like BigEndianReader(ByteArrayBE)

  "Big-endian byte array writers" should behave like BigEndianWriter(ByteArrayBE)


  "Little-endian byte arrays" should behave like PrimitiveSerializer(ByteArrayLE)
  it should behave like LittleEndianLoader(ByteArrayLE)
  it should behave like LittleEndianStorer(ByteArrayLE)

  "Little-endian byte array readers" should behave like LittleEndianReader(ByteArrayLE)

  "Little-endian byte array writers" should behave like LittleEndianWriter(ByteArrayLE)


  "Big-endian byte array framers" should behave like DataSerializer(ByteArrayBE)

  "Little-endian byte array framers" should behave like DataSerializer(ByteArrayLE)
}
