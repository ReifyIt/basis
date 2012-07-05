/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.generic.CanBuildFrom

import language.higherKinds

final class JSArray(protected val values: Array[JSValue]) extends JSValue { jsarray =>
  override protected type Root = JSArray
  
  def length: Int = values.length
  
  def apply(index: Int): JSValue = values(index)
  
  def updated(index: Int, value: JSValue): JSArray = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newValues = new Array[JSValue](length)
    System.arraycopy(values, 0, newValues, 0, index)
    newValues(index) = value
    System.arraycopy(values, index + 1, newValues, index + 1, length - (index + 1))
    new JSArray(newValues)
  }
  
  def :+ (value: JSValue): JSArray = {
    val newValues = new Array[JSValue](length + 1)
    System.arraycopy(values, 0, newValues, 0, length)
    newValues(length) = value
    new JSArray(newValues)
  }
  
  def +: (value: JSValue): JSArray = {
    val newValues = new Array[JSValue](length + 1)
    newValues(0) = value
    System.arraycopy(values, 0, newValues, 1, length)
    new JSArray(newValues)
  }
  
  def ++ (that: JSArray): JSArray = {
    if (that.length == 0) this
    else if (length == 0) that
    else {
      val newValues = new Array[JSValue](length + that.length)
      System.arraycopy(values, 0, newValues, 0, length)
      System.arraycopy(that.values, 0, newValues, length, that.length)
      new JSArray(newValues)
    }
  }
  
  override def \ [A <: JSValue](sel: PartialFunction[JSValue, A]): Selection[A] = new \ [A](sel)
  
  override def \\ [A <: JSValue](sel: PartialFunction[JSValue, A]): Selection[A] = new \\ [A](sel)
  
  @inline override def foreach[U](f: JSValue => U) {
    var i = 0
    while (i < length) {
      f(apply(i))
      i += 1
    }
  }
  
  @inline override def map(f: JSValue => JSValue): JSArray = {
    val newValues = new Array[JSValue](length)
    var i = 0
    while (i < length) {
      newValues(i) = f(apply(i))
      i += 1
    }
    new JSArray(newValues)
  }
  
  override def filter(p: JSValue => Boolean): JSArray = {
    val newValues = new Array[JSValue](length)
    var i = 0
    var k = 0
    while (i < length) {
      val value = apply(i)
      if (p(value)) {
        newValues(k) = value
        k += 1
      }
      i += 1
    }
    if (k < length) {
      val compactValues = new Array[JSValue](k)
      System.arraycopy(newValues, 0, compactValues, 0, k)
      new JSArray(compactValues)
    }
    else this
  }
  
  @inline override def foldLeft[A](z: A)(op: (A, JSValue) => A): A = {
    var result = z
    var i = 0
    while (i < length) {
      result = op(result, apply(i))
      i += 1
    }
    result
  }
  
  @inline override def foldRight[A](z: A)(op: (JSValue, A) => A): A = {
    var result = z
    var i = length - 1
    while (length >= 0) {
      result = op(apply(i), result)
      i -= 1
    }
    result
  }
  
  def convertTo[CC[_]](implicit bf: CanBuildFrom[Nothing, JSValue, CC[JSValue]]): CC[JSValue] = {
    val builder = bf()
    builder.sizeHint(length)
    var i = 0
    while (i < length) {
      builder += apply(i)
      i += 1
    }
    builder.result
  }
  
  override def write(s: Appendable) {
    s.append('[')
    if (0 < length) apply(0).write(s)
    var i = 1
    while (i < length) {
      s.append(',')
      apply(i).write(s)
      i += 1
    }
    s.append(']')
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: JSArray =>
      var equal = length == that.length
      var i = 0
      while (i < length && equal) {
        equal = apply(i).equals(that.apply(i))
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
      h = mix(h, apply(i).hashCode)
      i += 1
    }
    finalizeHash(h, length)
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    write(s)
    s.toString
  }
  
  private class \ [+A <: JSValue](sel: PartialFunction[JSValue, A]) extends Selection[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      while (i < jsarray.length) {
        val value = jsarray.apply(i)
        if (sel.isDefinedAt(value)) f(sel(value))
        i += 1
      }
    }
    
    override def map(f: A => JSValue): JSArray = {
      val newValues = new Array[JSValue](jsarray.length)
      var modified = false
      var i = 0
      while (i < jsarray.length) {
        val value = jsarray.apply(i)
        val newValue = if (sel.isDefinedAt(value)) f(sel(value)) else value
        newValues(i) = newValue
        modified ||= value eq newValue
        i += 1
      }
      if (modified) new JSArray(newValues) else jsarray
    }
    
    override def toString: String = "("+"JSArray"+" \\ "+ sel +")"
  }
  
  private class \\ [+A <: JSValue](sel: PartialFunction[JSValue, A]) extends Selection[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      while (i < jsarray.length) {
        val value = jsarray.apply(i)
        if (sel.isDefinedAt(value)) f(sel(value)) else value \\ sel foreach f
        i += 1
      }
    }
    
    override def map(f: A => JSValue): JSArray = {
      val newValues = new Array[JSValue](jsarray.length)
      var modified = false
      var i = 0
      while (i < jsarray.length) {
        val value = jsarray.apply(i)
        val newValue = if (sel.isDefinedAt(value)) f(sel(value)) else value \\ sel map f
        newValues(i) = newValue
        modified ||= value eq newValue
        i += 1
      }
      if (modified) new JSArray(newValues) else jsarray
    }
    
    override def toString: String = "("+"JSArray"+" \\\\ "+ sel +")"
  }
}

object JSArray {
  lazy val empty: JSArray = new JSArray(new Array[JSValue](0))
  
  def apply(values: JSValue*): JSArray = new JSArray(values.toArray[JSValue])
  
  def unapplySeq(jsarray: JSArray): Some[Seq[JSValue]] = Some(jsarray.convertTo[Seq])
  
  def parse(string: String): JSArray = {
    val parser = new model.JSONReader(string)
    parser.skipWhitespace()
    val jsarray = parser.parseJSONArray(JSON)
    parser.skipWhitespace()
    parser.parseEnd()
    jsarray
  }
  
  def newBuilder: JSArrayBuilder = new JSArrayBuilder
  
  implicit object canBuildFrom extends CanBuildFrom[Nothing, JSValue, JSArray] {
    def apply(): JSArrayBuilder = new JSArrayBuilder
    def apply(from: Nothing): JSArrayBuilder = new JSArrayBuilder
  }
  
  object unary_+ extends runtime.AbstractPartialFunction[Any, JSArray] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSArray]
    override def apply(x: Any): JSArray = x.asInstanceOf[JSArray]
    override def toString: String = "+JSArray"
  }
}
