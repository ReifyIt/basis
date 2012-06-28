/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.Builder

final class JSArrayBuilder(sizeHint: Int)
  extends JSON
    with model.JSONArrayBuilder[JSArray]
    with Builder[JSValue, JSArray] {
  
  def this() = this(0)
  
  private[this] var values: scala.Array[JSValue] =
    if (sizeHint > 0) new scala.Array[JSValue](sizeHint) else null
  
  private[this] var length: Int = 0
  
  private[this] def ensureCapacity(capacity: Int) {
    if (values == null) {
      val n = math.max(4, capacity)
      values = new scala.Array[JSValue](n)
    }
    else if (capacity > values.length) {
      var n = capacity - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n += 1
      val newValues = new scala.Array[JSValue](n)
      System.arraycopy(values, 0, newValues, 0, length)
      values = newValues
    }
  }
  
  override def sizeHint(size: Int): Unit = ensureCapacity(size)
  
  override def += (value: JSValue): this.type = {
    ensureCapacity(length + 1)
    values(length) = value
    length += 1
    this
  }
  
  override def result: JSArray = {
    if (length != 0) new JSArray(values, length)
    else JSArray.empty
  }
  
  override def clear() {
    values = null
    length = 0
  }
}
