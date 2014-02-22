//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

package object data {
  val BigEndian: BigEndian = new BigEndian
  val LittleEndian: LittleEndian = new LittleEndian

  type NativeEndian = NativeEndian.type

  /** The native byte order of the virtual machine. */
  implicit val NativeEndian: Endianness = {
    if (BigEndian.isNative) BigEndian
    else if (LittleEndian.isNative) LittleEndian
    else throw new AssertionError
  }

  implicit def LoaderToOps(data: Loader): LoaderOps = macro LoaderMacros.LoaderToOps
  implicit def StorerToOps(data: Storer): StorerOps = macro StorerMacros.StorerToOps
  implicit def ReaderToOps(data: Reader): ReaderOps = macro ReaderMacros.ReaderToOps
  implicit def WriterToOps(data: Writer): WriterOps = macro WriterMacros.WriterToOps

  /** Returns an address aligned to a power-of-two alignment.
    *
    * @param  base        the address to align.
    * @param  alignment   the required alignment.
    * @return the aligned address.
    */
  def align(base: Long, alignment: Long): Long = (base + (alignment - 1L)) & ~(alignment - 1L)
}
