/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.Builder

final class JSObjectBuilder(sizeHint: Int) extends Builder[(String, JSValue), JSObject] {
  def this() = this(16)
  
  private[this] var names = new Array[String](sizeHint)
  
  private[this] var values = new Array[JSValue](sizeHint)
  
  private[this] var length = 0
  
  private[this] def ensureCapacity(capacity: Int): Unit = if (capacity > names.length) {
    var newLength = 2 * names.length
    while (capacity > newLength) newLength *= 2
    val newNames = new Array[String](newLength)
    val newValues = new Array[JSValue](newLength)
    System.arraycopy(names, 0, newNames, 0, length)
    System.arraycopy(values, 0, newValues, 0, length)
    names = newNames
    values = newValues
  }
  
  override def sizeHint(size: Int): Unit = ensureCapacity(size)
  
  override def += (field: (String, JSValue)): this.type = {
    val (name, value) = field
    this += (name, value)
  }
  
  def += (name: String, value: JSValue): this.type = {
    ensureCapacity(length + 1)
    names(length) = name
    values(length) = value
    length += 1
    this
  }
  
  override def result: JSObject = if (length != 0) new JSObject(names, values, length) else JSObject.empty
  
  override def clear() {
    names = new Array[String](sizeHint)
    values = new Array[JSValue](sizeHint)
    length = 0
  }
}
