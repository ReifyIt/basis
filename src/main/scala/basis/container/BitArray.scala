/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

final class BitArray(val words: Array[Int]) extends AnyVal with AnyArray[Boolean] {
  override def length: Int = words(0)
  
  override def apply(index: Int): Boolean = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    ((words(1 + (index >> 5)) >>> (index & 0x1F)) & 1) == 1
  }
  
  override def update(index: Int, value: Boolean) {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val mask = 1 << (index & 0x1F)
    if (value) words(1 + (index >> 5)) |=  mask
    else       words(1 + (index >> 5)) &= ~mask
  }
  
  override def copy(length: Int = this.length): BitArray = {
    val newWords = new Array[Int](1 + (((length + 31) & ~31) >> 5))
    Array.copy(words, 1, newWords, 1, math.min(words.length, newWords.length) - 1)
    newWords(0) = length
    new BitArray(newWords)
  }
}

object BitArray {
  val Empty: BitArray = BitArray(0)
  
  def apply(length: Int): BitArray = {
    val words = new Array[Int](1 + (((length + 31) & ~31) >> 5))
    words(0) = length
    new BitArray(words)
  }
  
  final class Builder extends basis.collection.Builder[Any, Boolean] {
    override type Result = BitArray
    
    private[this] var array: BitArray = BitArray.Empty
    
    private[this] var aliased: Boolean = true
    
    private[this] var length: Int = 0
    
    private[this] def prepare(size: Int) {
      if (aliased || size > array.length) {
        array = array.copy(basis.collection.Builder.expand(16, size))
        aliased = false
      }
    }
    
    override def expect(count: Int) {
      if (length + count > array.length) {
        array = array.copy(length + count)
        aliased = false
      }
    }
    
    override def += (value: Boolean) {
      prepare(length + 1)
      array(length) = value
      length += 1
    }
    
    override def result: BitArray = {
      if (length != array.length) array = array.copy(length)
      aliased = true
      array
    }
    
    override def clear() {
      array = BitArray.Empty
      aliased = true
      length = 0
    }
  }
}
