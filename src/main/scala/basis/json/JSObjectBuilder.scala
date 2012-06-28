/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.Builder

final class JSObjectBuilder(sizeHint: Int)
  extends JSON
    with model.JSONObjectBuilder[JSObject]
    with Builder[(String, JSValue), JSObject] {
  
  def this() = this(0)
  
  private[this] var names: scala.Array[java.lang.String] =
    if (sizeHint > 0) new scala.Array[java.lang.String](sizeHint) else null
  
  private[this] var values: scala.Array[JSValue] =
    if (sizeHint > 0) new scala.Array[JSValue](sizeHint) else null
  
  private[this] var length: Int = 0
  
  private[this] def ensureCapacity(capacity: Int) {
    if (names == null || values == null) {
      val n = math.max(4, capacity)
      names = new scala.Array[java.lang.String](n)
      values = new scala.Array[JSValue](n)
    }
    else if (capacity > names.length || capacity > values.length) {
      var n = capacity - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n += 1
      val newNames = new scala.Array[java.lang.String](n)
      val newValues = new scala.Array[JSValue](n)
      System.arraycopy(names, 0, newNames, 0, length)
      System.arraycopy(values, 0, newValues, 0, length)
      names = newNames
      values = newValues
    }
  }
  
  override def sizeHint(size: Int): Unit = ensureCapacity(size)
  
  override def += (field: (java.lang.String, JSValue)): this.type = {
    val (name, value) = field
    this += (name, value)
  }
  
  def += (name: java.lang.String, value: JSValue): this.type = {
    ensureCapacity(length + 1)
    names(length) = name
    values(length) = value
    length += 1
    this
  }
  
  override def result: JSObject = {
    if (length != 0) new JSObject(names, values, length)
    else JSObject.empty
  }
  
  override def clear() {
    names = null
    values = null
    length = 0
  }
}
