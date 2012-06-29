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
  
  protected var length: Int = 0
  
  protected var names: scala.Array[java.lang.String] =
    if (sizeHint > 0) new scala.Array[java.lang.String](sizeHint) else null
  
  protected var values: scala.Array[JSValue] =
    if (sizeHint > 0) new scala.Array[JSValue](sizeHint) else null
  
  def += (name: java.lang.String, value: JSValue): this.type = {
    ensureCapacity(length + 1)
    names(length) = name
    values(length) = value
    length += 1
    this
  }
  
  override def += (field: (java.lang.String, JSValue)): this.type =
    this += (field._1, field._2)
  
  override def result: JSObject = {
    val newLength = length
    val newNames = names
    val newValues = values
    clear()
    if (newLength == 0) JSObject.empty
    else if (newLength == newNames.length && newLength == newValues.length)
      new JSObject(newNames, newValues)
    else {
      val compactNames = new scala.Array[java.lang.String](newLength)
      System.arraycopy(newNames, 0, compactNames, 0, newLength)
      val compactValues = new scala.Array[JSValue](newLength)
      System.arraycopy(newValues, 0, compactValues, 0, newLength)
      new JSObject(compactNames, compactValues)
    }
  }
  
  override def clear() {
    length = 0
    names = null
    values = null
  }
  
  override def sizeHint(size: Int): Unit = ensureCapacity(size)
  
  protected def ensureCapacity(capacity: Int) {
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
      System.arraycopy(names, 0, newNames, 0, length)
      names = newNames
      val newValues = new scala.Array[JSValue](n)
      System.arraycopy(values, 0, newValues, 0, length)
      values = newValues
    }
  }
}
