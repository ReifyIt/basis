//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

class ByteVectorSpec extends FlatSpec with ByteFactoryBehaviors with LoaderBehaviors with ReaderBehaviors with WriterBehaviors {
  override def suiteName = "ByteVector specification"

  "Big-endian byte vectors" should behave like BigEndianLoader(ByteVectorBE)

  "Big-endian byte vector readers" should behave like BigEndianReader(ByteVectorBE)

  "Big-endian byte vector writers" should behave like BigEndianWriter(ByteVectorBE)


  "Little-endian byte vectors" should behave like LittleEndianLoader(ByteVectorLE)

  "Little-endian byte vector readers" should behave like LittleEndianReader(ByteVectorLE)

  "Little-endian byte vector writers" should behave like LittleEndianWriter(ByteVectorLE)


  "Big-endian byte vector framers" should behave like DataSerializer(ByteVectorBE)

  "Little-endian byte vector framers" should behave like DataSerializer(ByteVectorLE)
}
