//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.collections._

abstract class FingerTrieData extends Family[FingerTrieData] with Loader {
  private[data] def prefix: Array[Byte]

  private[data] def branch: immutable.FingerTrieSeq[Array[Byte]]

  private[data] def suffix: Array[Byte]

  override def as[E <: Endianness](endian: E): FingerTrieData with ByteOrder[E]

  def drop(lower: Long): FingerTrieData with ByteOrder[Endian]

  def take(upper: Long): FingerTrieData with ByteOrder[Endian]

  def slice(lower: Long, upper: Long): FingerTrieData with ByteOrder[Endian]

  override def toArray: Array[Byte] = {
    if (size > Int.MaxValue.toLong)
      throw new UnsupportedOperationException("size exceeds maximum array capacity")
    var i = prefix.length
    val array = new Array[Byte](size.toInt)
    val segmenter = new FingerTrieDataArraySegmenter(this)
    System.arraycopy(prefix, 0, array, 0, i)
    while (!segmenter.isEmpty) {
      val segment = segmenter.head
      System.arraycopy(segment, 0, array, i, segment.length)
      i += segment.length
      segmenter.step()
    }
    System.arraycopy(suffix, 0, array, i, suffix.length)
    array
  }
}

object FingerTrieData extends ByteOrder[NativeEndian] with DataFactory[FingerTrieData with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: FingerTrieData with ByteOrder[NativeEndian] = {
    if (endian.isBig) FingerTrieDataBE.empty
    else if (endian.isLittle) FingerTrieDataLE.empty
    else throw new MatchError(endian)
  }.asInstanceOf[FingerTrieData with ByteOrder[NativeEndian]]

  override def from(data: Loader): FingerTrieData with ByteOrder[NativeEndian] = {
    if (data.isInstanceOf[FingerTrieData]) data.asInstanceOf[FingerTrieData].as(NativeEndian)
    else super.from(data)
  }

  implicit override def Framer: Framer with ByteOrder[NativeEndian] with State[FingerTrieData with ByteOrder[NativeEndian]] = {
    if (endian.isBig) FingerTrieDataBE.Framer
    else if (endian.isLittle) FingerTrieDataLE.Framer
    else throw new MatchError(endian)
  }.asInstanceOf[Framer with ByteOrder[NativeEndian] with State[FingerTrieData with ByteOrder[NativeEndian]]]

  override def toString: String = "FingerTrieData"
}

private[data] final class FingerTrieDataArraySegmenter(
    private[this] var prefix: Array[Byte],
    private[this] var branch: immutable.FingerTrieSeq[Array[Byte]],
    private[this] var suffix: Array[Byte],
    private[this] var inner: Iterator[Array[Byte]],
    private[this] var phase: Int)
  extends Iterator[Array[Byte]] {

  def this(data: FingerTrieData) =
    this(data.prefix, data.branch, data.suffix, null, if (data.size > 0L) 0 else 3)

  override def isEmpty: Boolean = phase >= 3

  override def head: Array[Byte] = phase match {
    case 0 => prefix
    case 1 => inner.head
    case 2 => suffix
    case _ => Iterator.empty.head
  }

  override def step(): Unit = phase match {
    case 0 =>
      prefix = null
      if (!branch.isEmpty) {
        inner = branch.iterator
        branch = null
        phase = 1
      }
      else if (suffix.length > 0) {
        branch = null
        phase = 2
      }
      else {
        branch = null
        suffix = null
        phase = 3
      }
    case 1 =>
      inner.step()
      if (inner.isEmpty) {
        inner = null
        phase = 2
      }
    case 2 =>
      suffix = null
      phase = 3
    case _ =>
      Iterator.empty.step()
  }

  override def dup: Iterator[Array[Byte]] =
    new FingerTrieDataArraySegmenter(prefix, branch, suffix, if (inner != null) inner.dup else null, phase)
}
