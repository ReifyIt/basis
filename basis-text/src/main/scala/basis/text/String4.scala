/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._
import basis.util._

/** A UTF-32 string.
  * 
  * @define collection  string
  */
final class String4(codeUnits: Array[Int]) extends UTF32 {
  override def length: Int = codeUnits.length
  
  override def get(index: Int): Int = codeUnits(index)
}

/** A factory for [[String4 UTF-32 strings]]. */
object String4 {
  val empty: String4 = new String4(new Array[Int](0))
  
  def apply(chars: CharSequence): String4 = {
    val s = new String4Builder
    s.append(chars)
    s.state
  }
  
  implicit def Builder: StringBuilder[Any] { type State = String4 } = new String4Builder
}

/** A builder for 32-bit Unicode strings in the UTF-32 encoding form.
  * Produces only well-formed code unit sequences. */
private[text] final class String4Iterator
    (string: String4, private[this] var index: Int)
  extends Iterator[Int] {
  
  override def isEmpty: Boolean = index >= string.length
  
  override def head: Int = {
    if (index >= string.length) throw new NoSuchElementException("Head of empty iterator.")
    string(index)
  }
  
  override def step() {
    if (index >= string.length) throw new UnsupportedOperationException("Empty iterator step.")
    index += 1
  }
  
  override def dup: Iterator[Int] = new String4Iterator(string, index)
}

/** A builder for 32-bit Unicode strings in the UTF-32 encoding form.
  * Produces only well-formed code unit sequences. */
private[text] final class String4Builder extends StringBuilder[Any] {
  override type State = String4
  
  private[this] var codeUnits: Array[Int] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newCodeUnits = new Array[Int](size)
    if (codeUnits != null) java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
    codeUnits = newCodeUnits
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > codeUnits.length) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def append(c: Int) {
    val n = size
    if ((c >= 0x0000 && c <= 0xD7FF) ||
        (c >= 0xE000 && c <= 0x10FFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      prepare(n + 1)
      codeUnits(n) = c
      size = n + 1
    }
    else { // invalid code point
      prepare(n + 1)
      codeUnits(n) = 0xFFFD
      size = n + 1
    }
  }
  
  override def expect(count: Int): this.type = {
    if (codeUnits == null || size + count > codeUnits.length) {
      resize(size + count)
      aliased = false
    }
    this
  }
  
  override def state: String4 = {
    if (codeUnits == null || size != codeUnits.length) resize(size)
    aliased = true
    new String4(codeUnits)
  }
  
  override def clear() {
    codeUnits = null
    aliased = true
    size = 0
  }
}
