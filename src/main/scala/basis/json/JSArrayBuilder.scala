/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.Builder

final class JSArrayBuilder(sizeHint: Int) extends Builder[JSValue, JSArray] {
  def this() = this(16)
  
  private[this] var values = new Array[JSValue](sizeHint)
  
  private[this] var length = 0
  
  private[this] def ensureCapacity(capacity: Int): Unit = if (capacity > values.length) {
    var newLength = 2 * values.length
    while (capacity > newLength) newLength *= 2
    val newValues = new Array[JSValue](newLength)
    System.arraycopy(values, 0, newValues, 0, length)
    values = newValues
  }
  
  override def sizeHint(size: Int): Unit = ensureCapacity(size)
  
  override def += (value: JSValue): this.type = {
    ensureCapacity(length + 1)
    values(length) = value
    length += 1
    this
  }
  
  override def result: JSArray = if (length != 0) new JSArray(values, length) else JSArray.empty
  
  override def clear() {
    values = new Array[JSValue](sizeHint)
    length = 0
  }
}
