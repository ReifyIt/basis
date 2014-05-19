//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._

trait ByteFactory[+Data] extends ByteOrder[Endianness] {
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

  implicit def Framer: Framer with ByteOrder[Endian] with State[Data]
}
