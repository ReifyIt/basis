/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

private[containers] final class BitArraySeq(words: Array[Int], override val length: Int) extends ArraySeq[Boolean] {
  override def isEmpty: Boolean = length == 0
  
  override def apply(index: Int): Boolean = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    ((words(index >> 5) >>> (31 - (index & 0x1F))) & 1) == 1
  }
  
  override def update[B >: Boolean](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Boolean]) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newWords = new Array[Int](words.length)
      java.lang.System.arraycopy(words, 0, newWords, 0, newWords.length)
      val mask = 1 << (31 - (index & 0x1F))
      if (elem.asInstanceOf[Boolean]) newWords(index >> 5) |=  mask
      else                            newWords(index >> 5) &= ~mask
      new BitArraySeq(newWords, length).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def iterator: Iterator[Boolean] = new BitArraySeqIterator(words, length)
}

private[containers] final class BitArraySeqIterator
    (words: Array[Int], private[this] var i: Int, n: Int)
  extends Iterator[Boolean] {
  
  def this(words: Array[Int], length: Int) = this(words, 0, length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Boolean = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    ((words(i >> 5) >>> (31 - (i & 0x1F))) & 1) == 1
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Boolean] = new BitArraySeqIterator(words, i, n)
}

private[containers] final class BitArraySeqBuilder extends Builder[Any, Boolean] {
  override type State = ArraySeq[Boolean]
  
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
  
  override def append(value: Boolean) {
    prepare(length + 1)
    val mask = 1 << (31 - (length & 0x1F))
    if (value) words(length >> 5) |=  mask
    //else     words(length >> 5) &= ~mask
    length += 1
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
