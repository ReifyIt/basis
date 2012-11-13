/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections.general._
import basis.util._

private[containers] final class BitArraySeq(words: Array[Int], override val length: Int) extends ArraySeq[Boolean] {
  override def isEmpty: Boolean = length == 0
  
  override def apply(index: Int): Boolean = {
    if (index < 0 || index >= length)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    ((words(index >> 5) >>> (31 - (index & 0x1F))) & 1) == 1
  }
  
  override def update[B >: Boolean](index: Int, value: B): ArraySeq[B] = value match {
    case bit: Boolean =>
      if (index < 0 || index >= length)
        throw new java.lang.IllegalArgumentException(index.toString)
      val newWords = words.clone
      val i = index >> 5
      val mask = 1 << (31 - (index & 0x1F))
      if (bit) newWords(i) |=  mask
      else     newWords(i) &= ~mask
      new BitArraySeq(newWords, length)
    case _ => super.update(index, value)
  }
  
  override def insert[B >: Boolean](index: Int, value: B): ArraySeq[B] = value match {
    case bit: Boolean =>
      if (index < 0 || index > length)
        throw new java.lang.IllegalArgumentException(index.toString)
      val newWords = new Array[Int](((length + 1 + 31) & ~31) >> 5)
      java.lang.System.arraycopy(words, 0, newWords, 0, index >> 5)
      var i = index >> 5
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
      new BitArraySeq(newWords, length + 1)
    case _ => super.insert(index, value)
  }
  
  override def remove(index: Int): ArraySeq[Boolean] = {
    if (index < 0 || index >= length)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    val newWords = new Array[Int](((length - 1 + 31) & ~31) >> 5)
    java.lang.System.arraycopy(words, 0, newWords, 0, index >> 5)
    var i = index >> 5
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
    new BitArraySeq(newWords, length - 1)
  }
  
  override def :+ [B >: Boolean](value: B): ArraySeq[B] = insert(length, value)
  
  override def +: [B >: Boolean](value: B): ArraySeq[B] = insert(0, value)
}

private[containers] final class BitArraySeqBuilder extends Builder[Any, Boolean, ArraySeq[Boolean]] {
  private[this] var words: Array[Int] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def capacity: Int = 32 * words.length
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newLength = ((size + 31) & ~31) >> 5
    if (words == null || words.length != newLength) {
      val newWords = new Array[Int](newLength)
      if (words != null) java.lang.System.arraycopy(words, 0, newWords, 0, words.length min newLength)
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
    if (value) words(length >> 5) |=  mask
    //else     words(length >> 5) &= ~mask
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (words == null || length + count > capacity) {
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ArraySeq[Boolean] = {
    if (words == null || length != capacity) resize(length)
    aliased = true
    new BitArraySeq(words, length)
  }
  
  override def clear() {
    words = null
    aliased = true
    length = 0
  }
}
