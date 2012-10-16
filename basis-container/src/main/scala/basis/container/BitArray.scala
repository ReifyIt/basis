/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class BitArray(val words: scala.Array[Int]) extends AnyVal with Array[Boolean] {
  override def length: Int = words(0)
  
  override def apply(index: Int): Boolean = {
    if (index < 0 || index >= length) throw new java.lang.IndexOutOfBoundsException(index.toString)
    ((words(1 + (index >> 5)) >>> (index & 0x1F)) & 1) == 1
  }
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update(index: Int, value: Boolean): BitArray = {
    val newWords = words.clone
    val mask = 1 << (index & 0x1F)
    if (value) words(1 + (index >> 5)) |=  mask
    else       words(1 + (index >> 5)) &= ~mask
    new BitArray(newWords)
  }
}

private[basis] object BitArray {
  val empty: BitArray = BitArray(0)
  
  def apply(length: Int): BitArray = {
    val words = new scala.Array[Int](1 + (((length + 31) & ~31) >> 5))
    words(0) = length
    new BitArray(words)
  }
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
    val mask = 1 << (length & 0x1F)
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
