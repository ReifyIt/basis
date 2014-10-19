//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

/** Low-level memory model.
  *
  * @contentDiagram hideNodes "basis.data.ByteOrder" "basis.data.DataFactory"
  */
package object data {
  /** Big-endian byte order of the host machine. */
  val BigEndian: BigEndian = {
    if (java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.BIG_ENDIAN) new BigEndianNative
    else new BigEndianSwapped
  }

  /** Little-endian byte order of the host machine. */
  val LittleEndian: LittleEndian = {
    if (java.nio.ByteOrder.nativeOrder eq java.nio.ByteOrder.LITTLE_ENDIAN) new LittleEndianNative
    else new LittleEndianSwapped
  }

  /** Native byte order of the host machine. */
  implicit val NativeEndian: NativeEndian = {
    if (BigEndian.isNative) BigEndian
    else if (LittleEndian.isNative) LittleEndian
    else throw new AssertionError
  }.asInstanceOf[NativeEndian]

  implicit def DataFactoryToOps[Data](factory: DataFactory[Data]): DataFactoryOps[Data] = macro DataMacros.DataFactoryToOps[Data]
  implicit def AllocatorToOps[Data](allocator: Allocator[Data]): AllocatorOps[Data] = macro DataMacros.AllocatorToOps[Data]

  implicit def LoaderToOps(data: Loader): LoaderOps[data.Family] = macro DataMacros.LoaderToOps
  implicit def ReaderToOps(data: Reader): ReaderOps = macro DataMacros.ReaderToOps
  implicit def StorerToOps(data: Storer): StorerOps = macro DataMacros.StorerToOps
  implicit def WriterToOps(data: Writer): WriterOps = macro DataMacros.WriterToOps

  /** Returns an address aligned to a power-of-two alignment.
    *
    * @param  base        the address to align.
    * @param  alignment   the required alignment.
    * @return the aligned address.
    */
  def align(base: Long, alignment: Long): Long = (base + (alignment - 1L)) & ~(alignment - 1L)
}
