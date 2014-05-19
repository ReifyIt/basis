//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

class ByteBufferSpec extends FlatSpec with ByteFactoryBehaviors with LoaderBehaviors with StorerBehaviors with ReaderBehaviors with WriterBehaviors {
  override def suiteName = "ByteBuffer specification"

  "Big-endian byte buffers" should behave like PrimitiveSerializer(ByteBufferBE)
  it should behave like BigEndianLoader(ByteBufferBE)
  it should behave like BigEndianStorer(ByteBufferBE)

  "Big-endian byte buffer readers" should behave like BigEndianReader(ByteBufferBE)

  "Big-endian byte buffer writers" should behave like BigEndianWriter(ByteBufferBE)


  "Little-endian byte buffers" should behave like PrimitiveSerializer(ByteBufferLE)
  it should behave like LittleEndianLoader(ByteBufferLE)
  it should behave like LittleEndianStorer(ByteBufferLE)

  "Little-endian byte buffer readers" should behave like LittleEndianReader(ByteBufferLE)

  "Little-endian byte buffer writers" should behave like LittleEndianWriter(ByteBufferLE)


  "Big-endian byte buffer framers" should behave like DataSerializer(ByteBufferBE)

  "Little-endian byte buffer framers" should behave like DataSerializer(ByteBufferLE)
}
