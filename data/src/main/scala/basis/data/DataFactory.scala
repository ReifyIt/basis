//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

trait DataFactory[+Data] extends ByteOrder[Endianness] {
  def empty: Data = Framer.state

  def apply(data: Array[Byte]): Data = {
    val size = data.length
    val framer = Framer.expect(size.toLong)
    var i = 0
    while (i < size) {
      framer.writeByte(data(i))
      i += 1
    }
    framer.state
  }

  def from(data: Loader): Data = {
    val size = data.size
    val framer = Framer.expect(size)
    var i = 0L
    while (i < size) {
      framer.writeByte(data.loadByte(i))
      i += 1L
    }
    framer.state
  }

  implicit def Framer: Framer with ByteOrder[Endian] with State[Data]
}
