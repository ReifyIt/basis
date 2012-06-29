/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.generic.CanBuildFrom

import language.higherKinds

final class JSObject(names: Array[String], values: Array[JSValue]) extends JSValue { jsobject =>
  override protected type Root = JSObject
  
  assert(names.length == values.length)
  
  def this(fields: Seq[(String, JSValue)]) = {
    this(new Array[String](fields.length), new Array[JSValue](fields.length))
    var i = 0
    while (i < fields.length) {
      val field = fields(i)
      names(i) = field._1
      values(i) = field._2
      i += 1
    }
  }
  
  def this(fields: Map[String, JSValue]) = this(fields.toSeq)
  
  def length: Int = names.length
  
  def getName(index: Int): String = names(index)
  
  def getValue(index: Int): JSValue = values(index)
  
  def apply(index: Int): (String, JSValue) = (getName(index), getValue(index))
  
  def apply(name: String): JSValue = {
    var i = 0
    while (i < length && !name.equals(getName(i))) i += 1
    if (i < length) getValue(i) else JSUndefined
  }
  
  def get(name: String): Option[JSValue] = {
    var i = 0
    while (i < length && !name.equals(getName(i))) i += 1
    if (i < length) Some(getValue(i)) else None
  }
  
  @inline def getOrElse(name: String, default: => JSValue): JSValue = {
    var i = 0
    while (i < length && !name.equals(getName(i))) i += 1
    if (i < length) getValue(i) else default
  }
  
  def contains(name: String): Boolean = {
    var i = 0
    while (i < length && !name.equals(getName(i))) i += 1
    i < length
  }
  
  def :+ (field: (String, JSValue)): JSObject = {
    val newLength = length + 1
    val newNames = new Array[String](newLength)
    System.arraycopy(names, 0, newNames, 0, length)
    newNames(length) = field._1
    val newValues = new Array[JSValue](newLength)
    System.arraycopy(values, 0, newValues, 0, length)
    newValues(length) = field._2
    new JSObject(newNames, newValues)
  }
  
  def +: (field: (String, JSValue)): JSObject = {
    val newLength = length + 1
    val newNames = new Array[String](newLength)
    newNames(0) = field._1
    System.arraycopy(names, 0, newNames, 1, length)
    val newValues = new Array[JSValue](newLength)
    newValues(0) = field._2
    System.arraycopy(values, 0, newValues, 1, length)
    new JSObject(newNames, newValues)
  }
  
  def + (field: (String, JSValue)): JSObject = {
    val name = field._1
    var i = 0
    while (i < length && !name.equals(getName(i))) i += 1
    if (i < length) {
      val newValues = new Array[JSValue](length)
      System.arraycopy(values, 0, newValues, 0, i)
      newValues(i) = field._2
      System.arraycopy(values, i + 1, newValues, i + 1, length - (i + 1))
      new JSObject(names, newValues)
    }
    else this :+ field
  }
  
  def - (name: String): JSObject = {
    var i = 0
    while (i < length && !name.equals(getName(i))) i += 1
    if (i < length) {
      val newLength = length - 1
      val newNames = new Array[String](newLength)
      System.arraycopy(names, 0, newNames, 0, i)
      System.arraycopy(names, i + 1, newNames, i, length - i)
      val newValues = new Array[JSValue](newLength)
      System.arraycopy(values, 0, newValues, 0, i)
      System.arraycopy(values, i + 1, newValues, i, length - i)
      new JSObject(newNames, newValues)
    }
    else this
  }
  
  override def \ (name: String): Selection[JSValue] = new SelectName(name)
  
  override def \ [A <: JSValue](sel: PartialFunction[JSValue, A]): Selection[A] = new \ [A](sel)
  
  override def \\ [A <: JSValue](sel: PartialFunction[JSValue, A]): Selection[A] = new \\ [A](sel)
  
  @inline override def foreach[U](f: JSValue => U) {
    var i = 0
    while (i < length) {
      f(getValue(i))
      i += 1
    }
  }
  
  @inline override def map(f: JSValue => JSValue): JSObject = {
    val newValues = new Array[JSValue](length)
    var i = 0
    while (i < length) {
      newValues(i) = f(getValue(i))
      i += 1
    }
    new JSObject(names, newValues)
  }
  
  override def filter(p: JSValue => Boolean): JSObject = {
    val newNames = new Array[String](length)
    val newValues = new Array[JSValue](length)
    var i = 0
    var k = 0
    while (i < length) {
      if (p(getValue(i))) {
        newNames(k) = getName(i)
        newValues(k) = getValue(i)
        k += 1
      }
      i += 1
    }
    if (k < length) {
      val compactNames = new Array[String](k)
      System.arraycopy(newNames, 0, compactNames, 0, k)
      val compactValues = new Array[JSValue](k)
      System.arraycopy(newValues, 0, compactValues, 0, k)
      new JSObject(compactNames, compactValues)
    }
    else this
  }
  
  @inline override def foldLeft[A](z: A)(op: (A, JSValue) => A): A = {
    var result = z
    var i = 0
    while (i < length) {
      result = op(result, getValue(i))
      i += 1
    }
    result
  }
  
  @inline override def foldRight[A](z: A)(op: (JSValue, A) => A): A = {
    var result = z
    var i = length - 1
    while (length >= 0) {
      result = op(getValue(i), result)
      i -= 1
    }
    result
  }
  
  def convertTo[CC[_]](implicit bf: CanBuildFrom[Nothing, (String, JSValue), CC[(String, JSValue)]]): CC[(String, JSValue)] = {
    val builder = bf()
    builder.sizeHint(length)
    var i = 0
    while (i < length) {
      builder += ((names(i), values(i)))
      i += 1
    }
    builder.result
  }
  
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
        equal = getName(i).equals(that.getName(i)) && getValue(i).equals(that.getValue(i))
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
      h = mix(mix(h, getName(i).hashCode), getValue(i).hashCode)
      i += 1
    }
    finalizeHash(h, length)
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    write(s)
    s.toString
  }
  
  private class SelectName(name: String) extends Selection[JSValue] {
    override def foreach[U](f: JSValue => U) {
      var i = 0
      while (i < jsobject.length) {
        if (name.equals(jsobject.getName(i))) f(jsobject.getValue(i))
        i += 1
      }
    }
    
    override def map(f: JSValue => JSValue): JSObject = {
      val newValues = new Array[JSValue](jsobject.length)
      var i = 0
      while (i < jsobject.length) {
        newValues(i) = if (name.equals(jsobject.getName(i))) f(jsobject.getValue(i)) else jsobject.getValue(i)
        i += 1
      }
      new JSObject(jsobject.names, newValues)
    }
    
    override def toString: String = "("+"_"+" \\ "+ name +")"
  }
  
  private class \ [+A <: JSValue](sel: PartialFunction[JSValue, A]) extends Selection[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      while (i < jsobject.length) {
        val jsvalue = jsobject.getValue(i)
        if (sel.isDefinedAt(jsvalue)) f(sel(jsvalue))
        i += 1
      }
    }
    
    override def map(f: A => JSValue): JSObject = {
      val newValues = new Array[JSValue](jsobject.length)
      var i = 0
      while (i < jsobject.length) {
        val jsvalue = jsobject.getValue(i)
        newValues(i) = if (sel.isDefinedAt(jsvalue)) f(sel(jsvalue)) else jsvalue
        i += 1
      }
      new JSObject(jsobject.names, newValues)
    }
    
    override def toString: String = "("+"_"+" \\ "+ sel +")"
  }
  
  private class \\ [+A <: JSValue](sel: PartialFunction[JSValue, A]) extends Selection[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      while (i < jsobject.length) {
        val jsvalue = jsobject.getValue(i)
        if (sel.isDefinedAt(jsvalue)) f(sel(jsvalue)) else jsvalue \\ sel foreach f
        i += 1
      }
    }
    
    override def map(f: A => JSValue): JSObject = {
      val newValues = new Array[JSValue](jsobject.length)
      var i = 0
      while (i < jsobject.length) {
        val jsvalue = jsobject.getValue(i)
        newValues(i) = if (sel.isDefinedAt(jsvalue)) f(sel(jsvalue)) else jsvalue \\ sel map f
        i += 1
      }
      new JSObject(jsobject.names, newValues)
    }
    
    override def toString: String = "("+"_"+" \\\\ "+ sel +")"
  }
}

object JSObject {
  lazy val empty: JSObject = new JSObject(new Array[String](0), new Array[JSValue](0))
  
  def apply(fields: (String, JSValue)*): JSObject = new JSObject(fields)
  
  def unapplySeq(jsobject: JSObject): Some[Seq[(String, JSValue)]] = Some(jsobject.convertTo[Seq])
  
  def parse(string: String): JSObject = {
    val parser = new model.JSONReader(string)
    parser.skipWhitespace()
    val jsobject = parser.parseJSONObject(JSON)
    parser.skipWhitespace()
    parser.parseEnd()
    jsobject
  }
  
  def newBuilder: JSObjectBuilder = new JSObjectBuilder
  
  implicit object canBuildFrom extends CanBuildFrom[Nothing, (String, JSValue), JSObject] {
    def apply(): JSObjectBuilder = new JSObjectBuilder
    def apply(from: Nothing): JSObjectBuilder = new JSObjectBuilder
  }
  
  object unary_+ extends runtime.AbstractPartialFunction[Any, JSObject] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSObject]
    override def apply(x: Any): JSObject = x.asInstanceOf[JSObject]
    override def toString: String = "+JSObject"
  }
}
