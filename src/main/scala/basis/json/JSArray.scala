/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

final class JSArray private[json] (values: Array[JSValue], val length: Int) extends JSValue {
  override protected type Root = JSArray
  
  assert(values.length >= length && length >= 0)
  
  def this(values: Seq[JSValue]) = this(values.toArray, values.length)
  
  override def foreach[U](f: JSValue => U) {
    var i = 0
    while (i < length) {
      f(values(i))
      i += 1
    }
  }
  
  override def map(f: JSValue => JSValue): JSArray = {
    val newValues = new Array[JSValue](length)
    var i = 0
    while (i < length) {
      newValues(i) = f(values(i))
      i += 1
    }
    new JSArray(newValues, length)
  }
  
  override def filter(p: JSValue => Boolean): JSArray = {
    val newValues = new Array[JSValue](length)
    var i = 0
    var k = 0
    while (i < length) {
      if (p(values(i))) {
        newValues(k) = values(i)
        k += 1
      }
      i += 1
    }
    new JSArray(newValues, k)
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
  
  def apply(index: Int): JSValue = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    values(index)
  }
  
  def updated(index: Int, value: JSValue): JSArray = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newValues = new Array[JSValue](length)
    System.arraycopy(values, 0, newValues, 0, index)
    newValues(index) = value
    System.arraycopy(values, index + 1, newValues, index + 1, length - (index + 1))
    new JSArray(newValues, length)
  }
  
  def :+ (value: JSValue): JSArray = {
    val newLength = length + 1
    val newValues = new Array[JSValue](newLength)
    System.arraycopy(values, 0, newValues, 0, length)
    newValues(length) = value
    new JSArray(newValues, newLength)
  }
  
  def +: (value: JSValue): JSArray = {
    val newLength = length + 1
    val newValues = new Array[JSValue](newLength)
    newValues(0) = value
    System.arraycopy(values, 0, newValues, 1, length)
    new JSArray(newValues, newLength)
  }
  
  def iterator: Iterator[JSValue] = new ValuesIterator
  
  override def write(s: Appendable) {
    s.append('[')
    if (0 < length) values(0).write(s)
    var i = 1
    while (i < length) {
      s.append(',')
      values(i).write(s)
      i += 1
    }
    s.append(']')
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: JSArray =>
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
    var h = -604447088
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
  
  private final class ValuesIterator extends Iterator[JSValue] {
    private[this] var index = 0
    override def hasNext: Boolean = index < JSArray.this.length
    override def next(): JSValue = {
      val value = JSArray.this.apply(index)
      index += 1
      value
    }
  }
}

object JSArray {
  lazy val empty: JSArray = new JSArray(new Array[JSValue](0), 0)
  
  def apply(values: JSValue*): JSArray = new JSArray(values)
  
  def unapplySeq(json: JSArray): Some[Seq[JSValue]] = Some(json.iterator.toSeq)
  
  def parse(string: String): JSArray = {
    val parser = new JSONReader[JSON.type](string)
    parser.skipWhitespace()
    parser.parseJSArray[JSON.type](JSON)
  }
  
  def newBuilder: JSArrayBuilder = new JSArrayBuilder
  
  implicit object canBuildFrom extends CanBuildFrom[Nothing, JSValue, JSArray] {
    def apply(): JSArrayBuilder = new JSArrayBuilder
    def apply(from: Nothing): JSArrayBuilder = new JSArrayBuilder
  }
  
  object unary_+ extends PartialFunction[Any, JSArray] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSArray]
    override def apply(x: Any): JSArray = x.asInstanceOf[JSArray]
    override def toString: String = "+JSArray"
  }
}
