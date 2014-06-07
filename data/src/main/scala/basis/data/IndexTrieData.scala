//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

abstract class IndexTrieData extends Family[IndexTrieData] with Loader {
  def mutateByte(address: Long, value: Byte): IndexTrieData with ByteOrder[Endian]

  def mutateShort(address: Long, value: Short): IndexTrieData with ByteOrder[Endian]

  def mutateInt(address: Long, value: Int): IndexTrieData with ByteOrder[Endian]

  def mutateLong(address: Long, value: Long): IndexTrieData with ByteOrder[Endian]

  def mutateFloat(address: Long, value: Float): IndexTrieData with ByteOrder[Endian]

  def mutateDouble(address: Long, value: Double): IndexTrieData with ByteOrder[Endian]
}

object IndexTrieData extends ByteOrder[NativeEndian] with DataFactory[IndexTrieData with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: IndexTrieData with ByteOrder[NativeEndian] = {
    if (endian.isBig) IndexTrieDataBE.empty
    else if (endian.isLittle) IndexTrieDataLE.empty
    else throw new MatchError(endian)
  }.asInstanceOf[IndexTrieData with ByteOrder[NativeEndian]]

  implicit override def Framer: Framer with ByteOrder[NativeEndian] with State[IndexTrieData with ByteOrder[NativeEndian]] = {
    if (endian.isBig) IndexTrieDataBE.Framer
    else if (endian.isLittle) IndexTrieDataLE.Framer
    else throw new MatchError(endian)
  }.asInstanceOf[Framer with ByteOrder[NativeEndian] with State[IndexTrieData with ByteOrder[NativeEndian]]]

  override def toString: String = "IndexTrieData"
}
