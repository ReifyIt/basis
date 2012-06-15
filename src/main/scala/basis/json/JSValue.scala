/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import language.implicitConversions

/** An optimized JSON tree model supporting jquery-style selectors.
  * 
  * ==Type constructors==
  * 
  * The `JSValue` algebraic data type has the following variants:
  * 
  *   - `JSObject` – a linear map of ''name'', ''value'' pairs.
  *   - `JSArray` – an indexed sequence of values.
  *   - `JSString` – a native `String` wrapper.
  *   - `JSNumber` – the root of the number types.
  *   - `JSInteger` – a native `Long` wrapper.
  *   - `JSDecimal` – a native `Double` wrapper.
  *   - `JSBoolean` – a native `Boolean` wrapper.
  *   - `JSNull` – the nullary constructor.
  *   - `JSUndefined` – an undefined value.
  * 
  * @author Chris Sachs
  */
abstract class JSValue { tree =>
  protected type Root >: this.type <: JSValue
  
  def foreach[U](f: JSValue => U): Unit = ()
  
  def map(f: JSValue => JSValue): Root = this
  
  def flatMap(f: JSValue => JSValue): Root = map(f)
  
  def filter(p: JSValue => Boolean): Root = this
  
  def withFilter(p: JSValue => Boolean): Root = filter(p)
  
  def foldLeft[A](z: A)(op: (A, JSValue) => A): A = z
  
  def foldRight[A](z: A)(op: (JSValue, A) => A): A = z
  
  /** Selects the values of all the fields of this object with the given name. */
  def \ (name: String): Vine[JSValue] = new SelectNone
  
  /** Selects all the children of this value matching the selector. */
  def \ [A <: JSValue](sel: PartialFunction[JSValue, A]): Vine[A] = new \ [A](sel)
  
  /** Selects all the descendants of this value matching the selector. */
  def \\ [A <: JSValue](sel: PartialFunction[JSValue, A]): Vine[A] = new \\ [A](sel)
  
  /** Writes the minimal textual form of this JSON value. */
  def write(s: Appendable): Unit
  
  protected class SelectNone extends Vine[JSValue] {
    override def foreach[U](f: JSValue => U): Unit = ()
    override def map(f: JSValue => JSValue): Root = tree
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
    
    /** Selects all the children of all the selected values matching the selector. */
    def \ [B <: JSValue](sel: PartialFunction[JSValue, B]): Vine[B] = new \ [B](sel)
    
    /** Selects all the descendents of all the selected values matching the selector. */
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
object JSValue {
  implicit def apply(fields: Map[String, JSValue]): JSObject = new JSObject(fields)
  
  implicit def apply(values: Seq[JSValue]): JSArray = new JSArray(values)
  
  implicit def apply(string: String): JSString = new JSString(string)
  
  implicit def apply(value: Int): JSInteger = new JSInteger(value)
  
  implicit def apply(value: Long): JSInteger = new JSInteger(value)
  
  implicit def apply(value: Float): JSDecimal = new JSDecimal(value)
  
  implicit def apply(value: Double): JSDecimal = new JSDecimal(value)
  
  implicit def apply(value: Boolean): JSBoolean = if (value) JSTrue else JSFalse
  
  def parse(string: String): JSValue = {
    val parser = new JSONReader[JSON.type](string)
    parser.skipWhitespace()
    val jsvalue = parser.parseJSValue[JSON.type](JSON)
    parser.skipWhitespace()
    parser.parseEnd()
    jsvalue
  }
  
  object unary_+ extends PartialFunction[Any, JSValue] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSValue]
    override def apply(x: Any): JSValue = x.asInstanceOf[JSValue]
    override def toString: String = "+JSValue"
  }
}
