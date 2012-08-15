/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.encoding
package utf32

trait Rope extends Any with basis.collection.IndexedSeq[Int] with Text {
  override type Kind <: Rope
  
  override def iterator: Text.Iterator = new Rope.Iterator(this, 0)
  
  override def size: Int
  
  def get(index: Int): Int
  
  override def length: Int = size
  
  override def apply(index: Int): Int = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c = get(index)
    if (c >= 0x0000 && c <= 0xD7FF ||
        c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
    else 0x4010FFFF
  }
  
  override def advance(index: Int): Int = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    index + 1
  }
  
  def ++ (that: Rope): Rope = new Rope.Concat(this, that)
}

object Rope {
  private final class Iterator(rope: Rope, private[this] var index: Int) extends Text.Iterator {
    override def hasNext: Boolean =
      index >= 0 && index < rope.size
    
    override def next(): Int = {
      val c = rope.get(index)
      index += 1
      c
    }
  }
  
  private final class Concat(prefix: Rope, suffix: Rope) extends Rope {
    private[this] val prefixSize: Int = prefix.size
    private[this] val suffixSize: Int = suffix.size
    
    override def size: Int = prefixSize + suffixSize
    
    override def get(index: Int): Int = {
      if (index < prefixSize) prefix.get(index)
      else suffix.get(index - prefixSize)
    }
  }
}
