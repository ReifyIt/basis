/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

final class JSObject private[json] (names: Array[String], values: Array[JSValue], val length: Int) extends JSValue {
  override protected type Root = JSObject
  
  assert(names.length >= length && values.length >= length && length >= 0)
  
  def this(fields: Seq[(String, JSValue)]) = {
    this(new Array[String](fields.length), new Array[JSValue](fields.length), fields.length)
    var i = 0
    while (i < fields.length) {
      val (name, value) = fields(i)
      names(i) = name
      values(i) = value
      i += 1
    }
  }
  
  def this(fields: Map[String, JSValue]) = this(fields.toSeq)
  
  override def foreach[U](f: JSValue => U) {
    var i = 0
    while (i < length) {
      f(values(i))
      i += 1
    }
  }
  
  override def map(f: JSValue => JSValue): JSObject = {
    val newValues = new Array[JSValue](length)
    var i = 0
    while (i < length) {
      newValues(i) = f(values(i))
      i += 1
    }
    new JSObject(names, newValues, length)
  }
  
  override def filter(p: JSValue => Boolean): JSObject = {
    val newNames = new Array[String](length)
    val newValues = new Array[JSValue](length)
    var i = 0
    var k = 0
    while (i < length) {
      if (p(values(i))) {
        newNames(k) = names(i)
        newValues(k) = values(i)
        k += 1
      }
      i += 1
    }
    new JSObject(newNames, newValues, k)
  }
  
  override def foldLeft[A](z: A)(op: (A, JSValue) => A): A = {
    var result = z
    var i = 0
    while (i < length) {
      result = op(result, values(i))
      i += 1
    }
    result
  }
  
  override def foldRight[A](z: A)(op: (JSValue, A) => A): A = {
    var result = z
    var i = length - 1
    while (length >= 0) {
      result = op(values(i), result)
      i -= 1
    }
    result
  }
  
  override def \ (name: String): Vine[JSValue] = new SelectName(name)
  
  def foreachField[U](f: (String, JSValue) => U) {
    var i = 0
    while (i < length) {
      f(names(i), values(i))
      i += 1
    }
  }
  
  def mapFields(f: (String, JSValue) => (String, JSValue)): JSObject = {
    val newNames = new Array[String](length)
    val newValues = new Array[JSValue](length)
    var i = 0
    while (i < length) {
      val (newName, newValue) = f(names(i), values(i))
      newNames(i) = newName
      newValues(i) = newValue
      i += 1
    }
    new JSObject(newNames, newValues, length)
  }
  
  def filterFields(p: (String, JSValue) => Boolean): JSObject = {
    val newNames = new Array[String](length)
    val newValues = new Array[JSValue](length)
    var i = 0
    var k = 0
    while (i < length) {
      if (p(names(i), values(i))) {
        newNames(k) = names(i)
        newValues(k) = values(i)
        k += 1
      }
      i += 1
    }
    new JSObject(newNames, newValues, k)
  }
  
  def apply(index: Int): (String, JSValue) = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (names(index), values(index))
  }
  
  def updated(index: Int, field: (String, JSValue)): JSObject = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val (name, value) = field
    val newNames = new Array[String](length)
    val newValues = new Array[JSValue](length)
    System.arraycopy(names, 0, newNames, 0, index)
    newNames(index) = name
    System.arraycopy(names, index + 1, newNames, index + 1, length - (index + 1))
    System.arraycopy(values, 0, newValues, 0, index)
    newValues(index) = value
    System.arraycopy(values, index + 1, newValues, index + 1, length - (index + 1))
    new JSObject(newNames, newValues, length)
  }
  
  def :+ (field: (String, JSValue)): JSObject = {
    val (name, value) = field
    val newLength = length + 1
    val newNames = new Array[String](newLength)
    val newValues = new Array[JSValue](newLength)
    System.arraycopy(names, 0, newNames, 0, length)
    System.arraycopy(values, 0, newValues, 0, length)
    newNames(length) = name
    newValues(length) = value
    new JSObject(newNames, newValues, newLength)
  }
  
  def +: (field: (String, JSValue)): JSObject = {
    val (name, value) = field
    val newLength = length + 1
    val newNames = new Array[String](newLength)
    val newValues = new Array[JSValue](newLength)
    newNames(0) = name
    newValues(0) = value
    System.arraycopy(names, 0, newNames, 1, length)
    System.arraycopy(values, 0, newValues, 1, length)
    new JSObject(newNames, newValues, newLength)
  }
  
  def contains(name: String): Boolean = {
    var i = 0
    while (i < length && !name.equals(names(i))) i += 1
    i < length
  }
  
  def apply(name: String): JSValue = {
    var i = 0
    while (i < length && !name.equals(names(i))) i += 1
    if (i < length) values(i) else JSUndefined
  }
  
  def get(name: String): Option[JSValue] = {
    var i = 0
    while (i < length && !name.equals(names(i))) i += 1
    if (i < length) {
      val value = values(i)
      if (value ne JSUndefined) Some(value) else None
    }
    else None
  }
  
  def getOrElse(name: String, default: => JSValue): JSValue = {
    var i = 0
    while (i < length && !name.equals(names(i))) i += 1
    if (i < length) values(i) else default
  }
  
  def updated(name: String, value: JSValue): JSObject = this + (name, value)
  
  def + (field: (String, JSValue)): JSObject = {
    val (name, value) = field
    var i = 0
    while (i < length && !name.equals(names(i))) i += 1
    if (i < length) {
      val newValues = new Array[JSValue](length)
      System.arraycopy(values, 0, newValues, 0, i)
      newValues(i) = value
      System.arraycopy(values, i + 1, newValues, i + 1, length - (i + 1))
      new JSObject(names, newValues, length)
    }
    else this :+ field
  }
  
  def - (name: String): JSObject = {
    var i = 0
    while (i < length && !name.equals(names(i))) i += 1
    if (i < length) {
      val newLength = length - 1
      val newNames = new Array[String](newLength)
      val newValues = new Array[JSValue](newLength)
      System.arraycopy(names, 0, newNames, 0, i)
      System.arraycopy(names, i + 1, newNames, i, length - i)
      System.arraycopy(values, 0, newValues, 0, i)
      System.arraycopy(values, i + 1, newValues, i, length - i)
      new JSObject(newNames, newValues, newLength)
    }
    else this
  }
  
  /** Returns an iterator over this object's fields. */
  def iterator: Iterator[(String, JSValue)] = new FieldsIterator
  
  /** Returns an iterator over this object's field names. */
  def namesIterator: Iterator[String] = new NamesIterator
  
  /** Returns an iterator over this object's field values. */
  def valuesIterator: Iterator[JSValue] = new ValuesIterator
  
  override def write(s: Appendable) {
    s.append('{')
    if (0 < length) {
      new JSString(names(0)).write(s)
      s.append(':')
      values(0).write(s)
    }
    var i = 1
    while (i < length) {
      s.append(',')
      new JSString(names(i)).write(s)
      s.append(':')
      values(i).write(s)
      i += 1
    }
    s.append('}')
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: JSObject =>
      var equal = length == that.length
      var i = 0
      while (i < length && equal) {
        equal = this(i).equals(that(i))
        i += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    var h = -1172193816
    var i = 0
    while (i < length) {
      h = mix(h, this(i).hashCode)
      i += 1
    }
    finalizeHash(h, length)
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    write(s)
    s.toString
  }
  
  private final class FieldsIterator extends Iterator[(String, JSValue)] {
    private[this] var index = 0
    override def hasNext: Boolean = index < JSObject.this.length
    override def next(): (String, JSValue) = {
      val field = JSObject.this.apply(index)
      index += 1
      field
    }
  }
  
  protected class SelectName(key: String) extends Vine[JSValue] {
    override def foreach[U](f: JSValue => U): Unit = JSObject.this.foreachField {
      case (name, value) if key.equals(name) => f(value)
      case field => ()
    }
    
    override def map(f: JSValue => JSValue): JSObject = JSObject.this.mapFields {
      case (name, value) if key.equals(name) => (name, f(value))
      case field => field
    }
    
    override def toString: String = "("+"_"+" \\ "+ key +")"
  }
  
  private final class NamesIterator extends Iterator[String] {
    private[this] var index = 0
    override def hasNext: Boolean = index < JSObject.this.length
    override def next(): String = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val name = JSObject.this.names(index)
      index += 1
      name
    }
  }
  
  private final class ValuesIterator extends Iterator[JSValue] {
    private[this] var index = 0
    override def hasNext: Boolean = index < JSObject.this.length
    override def next(): JSValue = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val value = JSObject.this.values(index)
      index += 1
      value
    }
  }
}

object JSObject {
  lazy val empty: JSObject = new JSObject(new Array[String](0), new Array[JSValue](0), 0)
  
  def apply(fields: (String, JSValue)*): JSObject = new JSObject(fields)
  
  def unapplySeq(json: JSObject): Some[Seq[(String, JSValue)]] = Some(json.iterator.toSeq)
  
  def parse(string: String): JSObject = {
    val parser = new JSONReader[JSON.type](string)
    parser.skipWhitespace()
    parser.parseJSObject[JSON.type](JSON)
  }
  
  def newBuilder: JSObjectBuilder = new JSObjectBuilder
  
  implicit object canBuildFrom extends CanBuildFrom[Nothing, (String, JSValue), JSObject] {
    def apply(): JSObjectBuilder = new JSObjectBuilder
    def apply(from: Nothing): JSObjectBuilder = new JSObjectBuilder
  }
  
  object unary_+ extends PartialFunction[Any, JSObject] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSObject]
    override def apply(x: Any): JSObject = x.asInstanceOf[JSObject]
    override def toString: String = "+JSObject"
  }
}
