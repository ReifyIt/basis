/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

import language.implicitConversions
import language.experimental.macros

/** An optimized JSON tree model supporting jquery-style selectors and
  * compile-time sting interpolation.
  * 
  * ==Data types==
  * 
  * The sealed `JSValue` algebraic data type has the following variants:
  * 
  *   - `JSObject` – a linear map of ''name'', ''value'' pairs.
  *   - `JSArray` – an indexed sequence of values.
  *   - `JSString` – a native `String` wrapper.
  *   - `JSNumber` – the root of the number types.
  *   - `JSInteger` – a native `Long` wrapper.
  *   - `JSDecimal` – a native `Double` wrapper.
  *   - `JSBoolean` – a native `Boolean` wrapper.
  *   - `JSNull` – the nullary constructor.
  * 
  * ==String interpolation==
  * 
  * The implicit `JSStringContext` class provides these string interpolators:
  * 
  *   - `json""` – statically parses any JSON value.
  *   - `jsobject""` – statically parses a JSON object.
  *   - `jsarray""` – statically parses a JSON array.
  * 
  * The embedded `JSStaticParser` object contains the macro implementations of
  * the string interpolators.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> import basis.json.JSONTree._
  * import basis.json.JSONTree._
  * 
  * scala> json""" [{}, [], "", 0, 0.0, true, false, null] """
  * res0: basis.json.JSONTree.JSArray = [{},[],"",0,0.0,true,false,null]
  * 
  * scala> val places = jsobject"""{"San Francisco":{"areaCode":415},"New York":{"areaCode":212}}"""
  * places: basis.json.JSONTree.JSObject = {"San Francisco":{"areaCode":415},"New York":{"areaCode":212}}
  * 
  * scala> for (JSInteger(areaCode) <- places \ "San Francisco" \ "areaCode") yield areaCode + 235
  * res1: basis.json.JSONTree.JSObject = {"San Francisco":{"areaCode":650},"New York":{"areaCode":212}}
  * 
  * scala> (jsarray""" ["Hello", 0, ", ", [null, "world", "!"]] """ \\ +JSString).toSeq.map(_.value).mkString
  * res2: String = Hello, world!
  * }}}
  */
object JSONTree extends JSONFactory {
  /** The universal selector. */
  object * extends PartialFunction[Any, JSValue] {
    override def isDefinedAt(value: Any): Boolean = value.isInstanceOf[JSValue]
    override def apply(value: Any): JSValue = value.asInstanceOf[JSValue]
    override def toString: String = "*"
  }
  
  sealed abstract class JSValue { tree =>
    protected type Root >: this.type <: JSValue
    
    def foreach[U](f: JSValue => U): Unit = ()
    
    def map(f: JSValue => JSValue): Root = this
    
    def flatMap(f: JSValue => JSValue): Root = map(f)
    
    def filter(p: JSValue => Boolean): Root = this
    
    def withFilter(p: JSValue => Boolean): Root = filter(p)
    
    def foldLeft[A](z: A)(op: (A, JSValue) => A): A = z
    
    def foldRight[A](z: A)(op: (JSValue, A) => A): A = z
    
    /** Selects the values of all the fields of this object with the given name. */
    def \ (name: String): Vine[JSValue] = new SelectAll
    
    /** Selects all the children of this value matching a selector. */
    def \ [A <: JSValue](sel: PartialFunction[JSValue, A]): Vine[A] = new \ [A](sel)
    
    /** Selects all the descendants of this value matching a selector. */
    def \\ [A <: JSValue](sel: PartialFunction[JSValue, A]): Vine[A] = new \\ [A](sel)
    
    /** Writes the minimal textual form of this JSON value. */
    def write(s: Appendable): Unit
    
    protected class SelectAll extends Vine[JSValue] {
      override def foreach[U](f: JSValue => U): Unit = tree foreach f
      override def map(f: JSValue => JSValue): Root = tree map f
    }
    
    protected class \ [+A <: JSValue](sel: PartialFunction[JSValue, A]) extends Vine[A] {
      override def foreach[U](f: A => U): Unit = tree foreach { value =>
        if (sel.isDefinedAt(value)) f(sel(value))
      }
      
      override def map(f: A => JSValue): Root = tree map { value =>
        if (sel.isDefinedAt(value)) f(sel(value)) else value
      }
      
      override def toString: String = "("+"_"+" \\ "+ sel +")"
    }
    
    protected class \\ [+A <: JSValue](sel: PartialFunction[JSValue, A]) extends Vine[A] {
      override def foreach[U](f: A => U): Unit = tree foreach { value =>
        if (sel.isDefinedAt(value)) f(sel(value)) else value \\ sel foreach f
      }
      
      override def map(f: A => JSValue): Root = tree map { value =>
        if (sel.isDefinedAt(value)) f(sel(value)) else value \\ sel map f
      }
      
      override def toString: String = "("+"_"+" \\\\ "+ sel +")"
    }
    
    abstract class Vine[+A <: JSValue] { vine =>
      def foreach[U](f: A => U)
      
      def map(f: A => JSValue): Root
      
      def flatMap(f: A => JSValue): Root = map(f)
      
      def filter(p: A => Boolean): Vine[A] = withFilter(p)
      
      def withFilter(p: A => Boolean): Vine[A] = new WithFilter(p)
      
      def foldLeft[B](z: B)(op: (B, A) => B): B = {
        var result = z
        for (value <- this) result = op(result, value)
        result
      }
      
      /** Selects the values of all the fields of all selected objects with the given name. */
      def \ (name: String): Vine[JSValue] = new SelectName(name)
      
      /** Selects all the children of all the selected values matching a selector. */
      def \ [B <: JSValue](sel: PartialFunction[JSValue, B]): Vine[B] = new \ [B](sel)
      
      /** Selects all the descendents of all the selected values matching a selector. */
      def \\ [B <: JSValue](sel: PartialFunction[JSValue, B]): Vine[B] = new \\ [B](sel)
      
      /** Returns a sequence containing the selected values. */
      def toSeq: Seq[A] = {
        val builder = Vector.newBuilder[A]
        for (value <- this) builder += value
        builder.result
      }
      
      protected class WithFilter(p: A => Boolean) extends Vine[A] {
        override def foreach[U](f: A => U): Unit = vine foreach (value => if (p(value)) f(value))
        override def map(f: A => JSValue): Root = vine map (value => if (p(value)) f(value) else value)
        override def toString: String = "("+ vine +" withFilter "+ p +")"
      }
      
      protected class SelectName(name: String) extends Vine[JSValue] {
        override def foreach[U](f: JSValue => U): Unit = vine foreach (value => value \ name foreach f)
        override def map(f: JSValue => JSValue): Root = vine map (value => value \ name map f)
        override def toString: String = "("+ vine +" \\ "+ name +")"
      }
      
      protected class \ [+B <: JSValue](sel: PartialFunction[JSValue, B]) extends Vine[B] {
        override def foreach[U](f: B => U): Unit = vine foreach (value => value \ sel foreach f)
        override def map(f: B => JSValue): Root = vine map (value => value \ sel map f)
        override def toString: String = "("+ vine +" \\ "+ sel +")"
      }
      
      protected class \\ [+B <: JSValue](sel: PartialFunction[JSValue, B]) extends Vine[B] {
        override def foreach[U](f: B => U): Unit = vine foreach (value => value \\ sel foreach f)
        override def map(f: B => JSValue): Root = vine map (value => value \\ sel map f)
        override def toString: String = "("+ vine +" \\\\ "+ sel +")"
      }
    }
  }
  
  /** Contains factory methods for and implicit conversions to JSON values. */
  object JSValue extends JSValueFactory {
    implicit def apply(s: String): JSString = new JSString(s)
    
    implicit def apply(n: Int): JSInteger = new JSInteger(n)
    
    implicit def apply(n: Long): JSInteger = new JSInteger(n)
    
    implicit def apply(x: Float): JSDecimal = new JSDecimal(x)
    
    implicit def apply(x: Double): JSDecimal = new JSDecimal(x)
  }
  
  
  final class JSObject private[JSONTree] (names: Array[String], values: Array[JSValue], val length: Int) extends JSValue {
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
      if (i < length) values(i) else throw new NoSuchElementException(name)
    }
    
    def get(name: String): Option[JSValue] = {
      var i = 0
      while (i < length && !name.equals(names(i))) i += 1
      if (i < length) Some(values(i)) else None
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
    
    /** Returns an iterator over this JSON object's fields. */
    def iterator: Iterator[(String, JSValue)] = new FieldsIterator
    
    /** Returns an iterator over this JSON object's field names. */
    def namesIterator: Iterator[String] = new NamesIterator
    
    /** Returns an iterator over this JSON object's field values. */
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
      import scala.util.MurmurHash3._
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
  
  object JSObject extends JSObjectFactory {
    override def newBuilder(sizeHint: Int): JSObjectBuilder = new JSObjectBuilder(sizeHint)
    
    override lazy val empty: JSObject = new JSObject(new Array[String](0), new Array[JSValue](0), 0)
    
    override def apply(fields: (String, JSValue)*): JSObject = new JSObject(fields)
    
    def unapplySeq(json: JSObject): Some[Seq[(String, JSValue)]] = Some(json.iterator.toSeq)
    
    implicit def canBuildFrom[A <: JSValue] = BuilderFactory.asInstanceOf[CanBuildFrom[Nothing, (String, A), JSObject]]
    
    private lazy val BuilderFactory = new BuilderFactory[Nothing]
    
    private class BuilderFactory[A <: JSValue] extends CanBuildFrom[Nothing, (String, A), JSObject] {
      def apply(): JSObjectBuilder = newBuilder()
      def apply(from: Nothing): JSObjectBuilder = newBuilder()
    }
    
    object unary_+ extends PartialFunction[Any, JSObject] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSObject]
      override def apply(x: Any): JSObject = x.asInstanceOf[JSObject]
      override def toString: String = "+JSObject"
    }
  }
  
  final class JSObjectBuilder(sizeHint: Int) extends super.JSObjectBuilder with Builder[(String, JSValue), JSObject] {
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
    
    override def += (name: String, value: JSValue): this.type = {
      ensureCapacity(length + 1)
      names(length) = name
      values(length) = value
      length += 1
      this
    }
    
    override def += (field: (String, JSValue)): this.type = {
      val (name, value) = field
      this += (name, value)
    }
    
    override def result: JSObject = if (length != 0) new JSObject(names, values, length) else JSObject.empty
    
    override def clear() {
      names = new Array[String](sizeHint)
      values = new Array[JSValue](sizeHint)
      length = 0
    }
  }
  
  
  final class JSArray private[JSONTree] (values: Array[JSValue], val length: Int) extends JSValue {
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
      import scala.util.MurmurHash3._
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
  
  object JSArray extends JSArrayFactory {
    override def newBuilder(sizeHint: Int): JSArrayBuilder = new JSArrayBuilder(sizeHint)
    
    override lazy val empty: JSArray = new JSArray(new Array[JSValue](0), 0)
    
    override def apply(values: JSValue*): JSArray = new JSArray(values)
    
    def unapplySeq(json: JSArray): Some[Seq[JSValue]] = Some(json.iterator.toSeq)
    
    implicit def canBuildFrom[A <: JSValue] = BuilderFactory.asInstanceOf[CanBuildFrom[Nothing, A, JSArray]]
    
    private lazy val BuilderFactory = new BuilderFactory[Nothing]
    
    private class BuilderFactory[A <: JSValue] extends CanBuildFrom[Nothing, A, JSArray] {
      def apply(): JSArrayBuilder = newBuilder()
      def apply(from: Nothing): JSArrayBuilder = newBuilder()
    }
    
    object unary_+ extends PartialFunction[Any, JSArray] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSArray]
      override def apply(x: Any): JSArray = x.asInstanceOf[JSArray]
      override def toString: String = "+JSArray"
    }
  }
  
  final class JSArrayBuilder(sizeHint: Int) extends super.JSArrayBuilder with Builder[JSValue, JSArray] {
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
  
  
  final class JSString(val value: String) extends JSValue {
    override protected type Root = JSString
    
    override def write(s: Appendable) {
      def toHexChar(h: Int): Char = (if (h < 10) '0' + h else 'A' + (h - 10)).toChar
      s.append('"')
      var c = '\0'
      var i = 0
      while (i < value.length) {
        val b = c
        c = value.charAt(i)
        c match {
          case '"'  => s.append('\\').append('"')
          case '\\' => s.append('\\').append('\\')
          case '\b' => s.append('\\').append('b')
          case '\f' => s.append('\\').append('f')
          case '\n' => s.append('\\').append('n')
          case '\r' => s.append('\\').append('r')
          case '\t' => s.append('\\').append('t')
          case '/'  if b == '<' => s.append('\\').append('/')
          case c    if (c >= '\u0000' && c < '\u001f') ||
                       (c >= '\u0080' && c < '\u00a0') ||
                       (c >= '\u2000' && c < '\u2100') =>
                       s.append('\\').append('u').
                         append(toHexChar((c >>> 12) & 0xF)).
                         append(toHexChar((c >>>  8) & 0xF)).
                         append(toHexChar((c >>>  4) & 0xF)).
                         append(toHexChar( c         & 0xF))
          case c    => s.append(c)
        }
        i += 1
      }
      s.append('"')
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: JSString => value.equals(that.value)
      case _ => false
    }
    
    override def hashCode: Int = value.hashCode
    
    override def toString: String = {
      val s = new java.lang.StringBuilder
      write(s)
      s.toString
    }
  }
  
  object JSString extends JSStringFactory {
    override def apply(s: String): JSString = new JSString(s)
    
    def unapply(json: JSString): Some[String] = Some(json.value)
    
    object unary_+ extends PartialFunction[Any, JSString] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSString]
      override def apply(x: Any): JSString = x.asInstanceOf[JSString]
      override def toString: String = "+JSString"
    }
  }
  
  
  sealed abstract class JSNumber extends JSValue {
    override protected type Root >: this.type <: JSNumber
    
    def toInt: Int
    
    def toLong: Long
    
    def toFloat: Float
    
    def toDouble: Double
  }
  
  object JSNumber extends JSNumberFactory {
    object unary_+ extends PartialFunction[Any, JSNumber] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSNumber]
      override def apply(x: Any): JSNumber = x.asInstanceOf[JSNumber]
      override def toString: String = "+JSNumber"
    }
  }
  
  
  final class JSInteger(value: Long) extends JSNumber {
    override protected type Root = JSInteger
    
    def this(s: String) = this(java.lang.Long.valueOf(s))
    
    override def write(s: Appendable): Unit = s.append(toString)
    
    override def toInt: Int = value.toInt
    
    override def toLong: Long = value
    
    override def toFloat: Float = value.toFloat
    
    override def toDouble: Double = value.toDouble
    
    override def equals(other: Any): Boolean = other match {
      case that: JSInteger => toLong == that.toLong
      case _ => false
    }
    
    override def hashCode: Int = value.##
    
    override def toString: String = value.toString
  }
  
  object JSInteger extends JSIntegerFactory {
    override def apply(s: String): JSInteger = new JSInteger(s)
    
    override def apply(n: Int): JSInteger = new JSInteger(n)
    
    override def apply(n: Long): JSInteger = new JSInteger(n)
    
    def unapply(json: JSInteger): Some[Long] = Some(json.toLong)
    
    object unary_+ extends PartialFunction[Any, JSInteger] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSInteger]
      override def apply(x: Any): JSInteger = x.asInstanceOf[JSInteger]
      override def toString: String = "+JSInteger"
    }
  }
  
  
  final class JSDecimal(value: Double) extends JSNumber {
    override protected type Root = JSDecimal
    
    def this(s: String) = this(java.lang.Double.valueOf(s))
    
    override def write(s: Appendable): Unit = s.append(toString)
    
    override def toInt: Int = value.toInt
    
    override def toLong: Long = value.toLong
    
    override def toFloat: Float = value.toFloat
    
    override def toDouble: Double = value
    
    override def equals(other: Any): Boolean = other match {
      case that: JSDecimal => toDouble == that.toDouble
      case _ => false
    }
    
    override def hashCode: Int = value.##
    
    override def toString: String =
      if (!java.lang.Double.isNaN(value) &&
          !java.lang.Double.isInfinite(value)) value.toString else "null"
  }
  
  object JSDecimal extends JSDecimalFactory {
    override def apply(s: String): JSDecimal = new JSDecimal(s)
    
    override def apply(x: Float): JSDecimal = new JSDecimal(x)
    
    override def apply(x: Double): JSDecimal = new JSDecimal(x)
    
    def unapply(json: JSDecimal): Some[Double] = Some(json.toDouble)
    
    object unary_+ extends PartialFunction[Any, JSDecimal] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSDecimal]
      override def apply(x: Any): JSDecimal = x.asInstanceOf[JSDecimal]
      override def toString: String = "+JSDecimal"
    }
  }
  
  
  final class JSBoolean private[JSONTree] (val value: Boolean) extends JSValue {
    override protected type Root = JSBoolean
    
    override def write(s: Appendable): Unit = s.append(toString)
    
    override def toString: String = if (value) "true" else "false"
  }
  
  /** Contains factory methods for JSON boolean values. */
  object JSBoolean {
    def apply(bool: Boolean): JSBoolean = if (bool) JSTrue else JSFalse
    
    def unapply(json: JSBoolean): Some[Boolean] = Some(json.value)
    
    object unary_+ extends PartialFunction[Any, JSBoolean] {
      override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSBoolean]
      override def apply(x: Any): JSBoolean = x.asInstanceOf[JSBoolean]
      override def toString: String = "+JSBoolean"
    }
  }
  
  override val JSTrue = new JSBoolean(true)
  
  override val JSFalse = new JSBoolean(false)
  
  
  final class JSNull private[JSONTree] extends JSValue {
    override protected type Root = JSNull
    
    override def write(s: Appendable): Unit = s.append(toString)
    
    override def toString: String = "null"
  }
  
  override val JSNull = new JSNull
  
  
  /** Provides `json`, `jsobject`, and `jsarray` string interpolators. */
  implicit class JSStringContext(context: StringContext) {
    def json(args: JSValue*): JSValue = macro JSStaticParser.parseJSValue
    
    def jsobject(args: JSValue*): JSObject = macro JSStaticParser.parseJSObject
    
    def jsarray(args: JSValue*): JSArray = macro JSStaticParser.parseJSArray
  }
  
  /** Contains string interpolation macro implementations. */
  object JSStaticParser {
    import scala.reflect.makro.Context
    
    def newPrefixParser(c: Context)(args: Seq[c.Expr[JSValue]])
      : JSONParser[JSONExpr[c.mirror.type, JSONTree.type] with Singleton] = {
      import c.mirror._
      val Apply(_, List(Apply(_, literals))) = c.prefix.tree
      val parts = literals map { case Literal(Constant(part: String)) => part }
      val jsonExpr = new JSONExpr[c.mirror.type, JSONTree.type](c.mirror)(reify(JSONTree))
      new JSONParser.Interpolating[jsonExpr.type](jsonExpr, parts)(args)
    }
    
    def parseJSValue(c: Context)(args: c.Expr[JSValue]*): c.Expr[JSValue] = {
      val parser = newPrefixParser(c)(args)
      parser.skipWhitespace()
      parser.parseJSValue()
    }
    
    def parseJSObject(c: Context)(args: c.Expr[JSValue]*): c.Expr[JSObject] = {
      val parser = newPrefixParser(c)(args)
      parser.skipWhitespace()
      parser.parseJSObject()
    }
    
    def parseJSArray(c: Context)(args: c.Expr[JSValue]*): c.Expr[JSArray] = {
      val parser = newPrefixParser(c)(args)
      parser.skipWhitespace()
      parser.parseJSArray()
    }
  }
}
