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
  
  private[basis] def update(index: Int, value: Boolean) {
    if (index < 0 || index >= length) throw new java.lang.IndexOutOfBoundsException(index.toString)
    val mask = 1 << (index & 0x1F)
    if (value) words(1 + (index >> 5)) |=  mask
    else       words(1 + (index >> 5)) &= ~mask
  }
  
  private[basis] def copy(length: Int): BitArray = {
    val newWords = new scala.Array[Int](1 + (((length + 31) & ~31) >> 5))
    java.lang.System.arraycopy(words, 1, newWords, 1, (words.length min newWords.length) - 1)
    newWords(0) = length
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
  
  private[this] var array: BitArray = BitArray.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      array = array.copy(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: Boolean): this.type = {
    prepare(length + 1)
    array(length) = value
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    this
  }
  
  override def state: BitArray = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = BitArray.empty
    aliased = true
    length = 0
  }
}
