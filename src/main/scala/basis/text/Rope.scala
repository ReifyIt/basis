/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

trait Rope extends Any with CharSeq {
  override type Kind <: Rope
  
  override def size: Int
  
  override def apply(index: Int): Char
  
  override def advance(index: Int): Int
  
  def ++ (that: Rope): Rope = new Rope.Concat(this, that)
}

object Rope {
  private final class Concat(prefix: Rope, suffix: Rope) extends Rope {
    private[this] val prefixSize: Int = prefix.size
    private[this] val suffixSize: Int = suffix.size
    
    override def size: Int = prefixSize + suffixSize
    
    override def apply(index: Int): Char = {
      if (index < prefixSize) prefix.apply(index)
      else suffix.apply(index - prefixSize)
    }
    
    override def advance(index: Int): Int = {
      if (index < prefixSize) prefix.advance(index)
      else suffix.advance(index - prefixSize)
    }
  }
}
