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
  
  protected var length: Int = 0
  
  protected var values: scala.Array[JSValue] =
    if (sizeHint > 0) new scala.Array[JSValue](sizeHint) else null
  
  override def += (value: JSValue): this.type = {
    ensureCapacity(length + 1)
    values(length) = value
    length += 1
    this
  }
  
  override def result: JSArray = {
    val newLength = length
    val newValues = values
    clear()
    if (newLength == 0) JSArray.empty
    else if (newLength == newValues.length) new JSArray(newValues)
    else {
      val compactValues = new scala.Array[JSValue](newLength)
      System.arraycopy(newValues, 0, compactValues, 0, newLength)
      new JSArray(compactValues)
    }
  }
  
  override def clear() {
    length = 0
    values = null
  }
  
  override def sizeHint(size: Int): Unit = ensureCapacity(size)
  
  protected def ensureCapacity(capacity: Int) {
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
}
