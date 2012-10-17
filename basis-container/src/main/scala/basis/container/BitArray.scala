/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class BitArray(val words: scala.Array[Int]) extends AnyVal with Array[Boolean] {
  override def isEmpty: Boolean = words(0) == 0
  
  override def length: Int = words(0)
  
  override def apply(index: Int): Boolean = {
    if (index < 0 || index >= length)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    ((words(1 + (index >> 5)) >>> (31 - (index & 0x1F))) & 1) == 1
  }
  
  /** Returns a copy of this array with a new `bit` at `index`. */
  def update(index: Int, bit: Boolean): BitArray = {
    val newWords = words.clone
    val i = 1 + (index >> 5)
    val mask = 1 << (31 - (index & 0x1F))
    if (bit) newWords(i) |=  mask
    else     newWords(i) &= ~mask
    new BitArray(newWords)
  }
  
  /** Returns a copy of this array with a new `bit` inserted at `index`. */
  def insert(index: Int, bit: Boolean): BitArray = {
    if (index < 0 || index > length)
      throw new java.lang.IllegalArgumentException(index.toString)
    val newWords = new scala.Array[Int](1 + (((length + 1 + 31) & ~31) >> 5))
    newWords(0) = length + 1
    java.lang.System.arraycopy(words, 1, newWords, 1, index >> 5)
    var i = 1 + (index >> 5)
    val l = 0xFFFFFFFF >>> (index & 0x1F)
    val h = ~l
    val b = (if (bit) 1 else 0) << (31 - (index & 0x1F))
    var w = if (i < words.length) words(i) else 0
    newWords(i) = (w & h) | b | ((w & l) >>> 1)
    var t = w & 1
    i += 1
    while (i < words.length) {
      w = words(i)
      newWords(i) = (t << 31) | (w >>> 1)
      t = w & 1
      i += 1
    }
    if (i < newWords.length) newWords(i) = t << 31
    new BitArray(newWords)
  }
  
  /** Returns a copy of this array with the bit at `index` removed. */
  def remove(index: Int): BitArray = {
    if (index < 0 || index >= length)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    val newWords = new scala.Array[Int](1 + (((length - 1 + 31) & ~31) >> 5))
    newWords(0) = length - 1
    java.lang.System.arraycopy(words, 1, newWords, 1, index >> 5)
    var i = 1 + (index >> 5)
    val l = 0xFFFFFFFF >>> (index & 0x1F)
    val h = ~l
    var w = words(i)
    if (i < newWords.length) newWords(i) = (w & h) | ((w & (l >>> 1)) << 1)
    i += 1
    while (i < newWords.length) {
      w = words(i)
      newWords(i - 1) |= w >>> 31
      newWords(i) = w << 1
      i += 1
    }
    if (i < words.length) newWords(i - 1) |= words(i) >>> 31
    new BitArray(newWords)
  }
  
  /** Returns a copy of this array with `bit` appended. */
  def :+ (bit: Boolean): BitArray = insert(length, bit)
  
  /** Returns a copy of this array with `bit` prepended. */
  def +: (bit: Boolean): BitArray = insert(0, bit)
}

object BitArray {
  val empty: BitArray = new BitArray(new scala.Array[Int](1))
}

final class BitArrayBuffer extends Buffer[Any, Boolean] {
  override type State = BitArray
  
  private[this] var words: scala.Array[Int] = BitArray.empty.words
  
  private[this] var aliased: Boolean = true
  
  private[this] def length: Int = words(0)
  
  private[this] def length_= (length: Int): Unit = words(0) = length
  
  private[this] def capacity: Int = 32 * (words.length - 1)
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newLength = 1 + (((size + 31) & ~31) >> 5)
    if (words.length != newLength) {
      val newWords = new scala.Array[Int](newLength)
      java.lang.System.arraycopy(words, 0, newWords, 0, words.length min newLength)
      words = newWords
    }
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > capacity) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: Boolean): this.type = {
    prepare(length + 1)
    val mask = 1 << (31 - (length & 0x1F))
    if (value) words(1 + (length >> 5)) |=  mask
    //else     words(1 + (length >> 5)) &= ~mask
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (length + count > capacity) {
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: BitArray = {
    if (length != capacity) resize(length)
    aliased = true
    new BitArray(words)
  }
  
  override def clear() {
    words = BitArray.empty.words
    aliased = true
    length = 0
  }
}
