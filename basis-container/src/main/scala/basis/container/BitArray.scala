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
  
  def update(index: Int, value: Boolean) {
    if (index < 0 || index >= length) throw new java.lang.IndexOutOfBoundsException(index.toString)
    val mask = 1 << (index & 0x1F)
    if (value) words(1 + (index >> 5)) |=  mask
    else       words(1 + (index >> 5)) &= ~mask
  }
  
  def copy(length: Int = this.length): BitArray = {
    val newWords = new scala.Array[Int](1 + (((length + 31) & ~31) >> 5))
    scala.Array.copy(words, 1, newWords, 1, scala.math.min(words.length, newWords.length) - 1)
    newWords(0) = length
    new BitArray(newWords)
  }
}

object BitArray {
  val empty: BitArray = BitArray(0)
  
  def apply(length: Int): BitArray = {
    val words = new scala.Array[Int](1 + (((length + 31) & ~31) >> 5))
    words(0) = length
    new BitArray(words)
  }
}
