//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

trait ArrayData extends Any with Family[ArrayData] with Loader with Storer {
  def drop(lower: Long): ArrayData with ByteOrder[Endian]

  def take(upper: Long): ArrayData with ByteOrder[Endian]

  def slice(lower: Long, upper: Long): ArrayData with ByteOrder[Endian]

  def toArray: Array[Byte]
}

object ArrayData extends ByteOrder[NativeEndian] with Allocator[ArrayData with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: ArrayData with ByteOrder[NativeEndian] = {
    if (endian.isBig) ArrayDataBE.empty
    else if (endian.isLittle) ArrayDataLE.empty
    else throw new MatchError(endian)
  }.asInstanceOf[ArrayData with ByteOrder[NativeEndian]]

  override def apply(data: Array[Byte]): ArrayData with ByteOrder[NativeEndian] = {
    if (endian.isBig) ArrayDataBE(data)
    else if (endian.isLittle) ArrayDataLE(data)
    else throw new MatchError(endian)
  }.asInstanceOf[ArrayData with ByteOrder[NativeEndian]]

  override def apply(size: Long): ArrayData with ByteOrder[NativeEndian] = {
    if (endian.isBig) ArrayDataBE(size)
    else if (endian.isLittle) ArrayDataLE(size)
    else throw new MatchError(endian)
  }.asInstanceOf[ArrayData with ByteOrder[NativeEndian]]

  implicit override def Framer: Framer with ByteOrder[NativeEndian] with State[ArrayData with ByteOrder[NativeEndian]] = {
    if (endian.isBig) ArrayDataBE.Framer
    else if (endian.isLittle) ArrayDataLE.Framer
    else throw new MatchError(endian)
  }.asInstanceOf[Framer with ByteOrder[NativeEndian] with State[ArrayData with ByteOrder[NativeEndian]]]

  override def toString: String = "ArrayData"
}
