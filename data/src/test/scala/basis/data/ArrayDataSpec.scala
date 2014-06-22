//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

class ArrayDataSpec
  extends FlatSpec
  with DataFactoryBehaviors
  with LoaderBehaviors
  with StorerBehaviors
  with ReaderBehaviors
  with WriterBehaviors {

  override def suiteName = "ArrayData specification"

  "Big-endian array data" should behave like PrimitiveSerializer(ArrayDataBE)
  it should behave like BigEndianLoader(ArrayDataBE)
  it should behave like BigEndianStorer(ArrayDataBE)

  "Big-endian array data readers" should behave like BigEndianReader(ArrayDataBE)

  "Big-endian array data writers" should behave like BigEndianWriter(ArrayDataBE)


  "Little-endian array data" should behave like PrimitiveSerializer(ArrayDataLE)
  it should behave like LittleEndianLoader(ArrayDataLE)
  it should behave like LittleEndianStorer(ArrayDataLE)

  "Little-endian array data readers" should behave like LittleEndianReader(ArrayDataLE)

  "Little-endian array data writers" should behave like LittleEndianWriter(ArrayDataLE)


  "Big-endian array data framers" should behave like DataSerializer(ArrayDataBE)

  "Little-endian array data framers" should behave like DataSerializer(ArrayDataLE)
}
