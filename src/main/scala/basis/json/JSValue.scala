/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.CanBuildFrom

import language.higherKinds
import language.implicitConversions

/** An optimized JSON algebraic data type.
  * 
  * @author Chris Sachs
  */
abstract class JSValue { jsvalue =>
  protected type Root >: this.type <: JSValue
  
  /** Writes the minimal textual form of this JSON value. */
  def write(s: Appendable): Unit
  
  /** Selects all the fields of this object whose names match the given name. */
  def \ (name: String): Selection[JSValue] = new EmptySelection[JSValue]
  
  /** Selects all the children of this value that match the given selector. */
  def \ [A <: JSValue](sel: PartialFunction[JSValue, A]): Selection[A] = new EmptySelection[A]
  
  /** Selects all the descendants of this value that match the given selector. */
  def \\ [A <: JSValue](sel: PartialFunction[JSValue, A]): Selection[A] = new EmptySelection[A]
  
  def foreach[U](f: JSValue => U): Unit = ()
  
  def map(f: JSValue => JSValue): Root = this
  
  def flatMap(f: JSValue => JSValue): Root = map(f)
  
  def filter(p: JSValue => Boolean): Root = this
  
  def withFilter(p: JSValue => Boolean): Root = filter(p)
  
  def foldLeft[A](z: A)(op: (A, JSValue) => A): A = z
  
  def foldRight[A](z: A)(op: (JSValue, A) => A): A = z
  
  abstract class Selection[+A <: JSValue] { selection =>
    /** Selects all the fields of all the selected objects whose names match the given name. */
    def \ (name: String): Selection[JSValue] = new SelectName(name)
    
    /** Selects all the children of all the selected values that match the given selector. */
    def \ [B <: JSValue](sel: PartialFunction[JSValue, B]): Selection[B] = new \ [B](sel)
    
    /** Selects all the descendents of all the selected values that match the given selector. */
    def \\ [B <: JSValue](sel: PartialFunction[JSValue, B]): Selection[B] = new \\ [B](sel)
    
    def foreach[U](f: A => U)
    
    def map(f: A => JSValue): Root
    
    def flatMap(f: A => JSValue): Root = map(f)
    
    def filter(p: A => Boolean): Selection[A] = withFilter(p)
    
    def withFilter(p: A => Boolean): Selection[A] = new WithFilter(p)
    
    def foldLeft[B](z: B)(op: (B, A) => B): B = {
      var result = z
      for (value <- this) result = op(result, value)
      result
    }
    
    def take(n: Int): Selection[A] = new Take(n)
    
    def drop(n: Int): Selection[A] = new Drop(n)
    
    def slice(from: Int, until: Int): Selection[A] = new Slice(from, until)
    
    def convertTo[CC[_]](implicit bf: CanBuildFrom[Nothing, A, CC[A @uncheckedVariance]]): CC[A @uncheckedVariance] = {
      val builder = bf()
      for (value <- this) builder += value
      builder.result
    }
    
    private class SelectName(name: String) extends Selection[JSValue] {
      override def foreach[U](f: JSValue => U): Unit = selection foreach (value => value \ name foreach f)
      override def map(f: JSValue => JSValue): Root = selection map (value => value \ name map f)
      override def toString: String = "("+ selection +" \\ "+ name +")"
    }
    
    private class \ [+B <: JSValue](sel: PartialFunction[JSValue, B]) extends Selection[B] {
      override def foreach[U](f: B => U): Unit = selection foreach (value => value \ sel foreach f)
      override def map(f: B => JSValue): Root = selection map (value => value \ sel map f)
      override def toString: String = "("+ selection +" \\ "+ sel +")"
    }
    
    private class \\ [+B <: JSValue](sel: PartialFunction[JSValue, B]) extends Selection[B] {
      override def foreach[U](f: B => U): Unit = selection foreach (value => value \\ sel foreach f)
      override def map(f: B => JSValue): Root = selection map (value => value \\ sel map f)
      override def toString: String = "("+ selection +" \\\\ "+ sel +")"
    }
    
    private class WithFilter(p: A => Boolean) extends Selection[A] {
      override def foreach[U](f: A => U): Unit = selection foreach (value => if (p(value)) f(value))
      override def map(f: A => JSValue): Root = selection map (value => if (p(value)) f(value) else value)
      override def toString: String = "("+ selection +" withFilter "+ p +")"
    }
    
    private class Take(n: Int) extends Selection[A] {
      assert(0 <= n)
      
      override def foreach[U](f: A => U) {
        var i = 0
        selection foreach { value =>
          if (i < n) f(value)
          i += 1
        }
      }
      
      override def map(f: A => JSValue): Root = {
        var i = 0
        selection map { value =>
          val temp = if (i < n) f(value) else value
          i += 1
          temp
        }
      }
      
      override def toString: String = "("+ selection +" take "+ n +")"
    }
    
    private class Drop(n: Int) extends Selection[A] {
      assert(0 <= n)
      
      override def foreach[U](f: A => U) {
        var i = 0
        selection foreach { value =>
          if (i >= n) f(value)
          i += 1
        }
      }
      
      override def map(f: A => JSValue): Root = {
        var i = 0
        selection map { value =>
          val temp = if (i >= n) f(value) else value
          i += 1
          temp
        }
      }
      
      override def toString: String = "("+ selection +" drop "+ n +")"
    }
    
    private class Slice(from: Int, until: Int) extends Selection[A] {
      assert(0 <= from && from < until)
      
      override def foreach[U](f: A => U) {
        var i = 0
        selection foreach { value =>
          if (i >= from && i < until) f(value)
          i += 1
        }
      }
      
      override def map(f: A => JSValue): Root = {
        var i = 0
        selection map { value =>
          val temp = if (i >= from && i < until) f(value) else value
          i += 1
          temp
        }
      }
      
      override def toString: String = selection +"."+"slice"+"("+ from +", "+ until +")"
    }
  }
  
  private class EmptySelection[+A <: JSValue] extends Selection[A] {
    override def \ (name: String): Selection[JSValue] = this
    override def \ [B <: JSValue](sel: PartialFunction[JSValue, B]): Selection[B] = new EmptySelection[B]
    override def \\ [B <: JSValue](sel: PartialFunction[JSValue, B]): Selection[B] = new EmptySelection[B]
    override def foreach[U](f: A => U): Unit = ()
    override def map(f: A => JSValue): Root = jsvalue
    override def withFilter(p: A => Boolean): Selection[A] = this
    override def foldLeft[B](z: B)(op: (B, A) => B): B = z
    override def convertTo[CC[_]](implicit bf: CanBuildFrom[Nothing, A, CC[A] @uncheckedVariance]): CC[A @uncheckedVariance] = bf().result
    override def toString: String = "empty selection"
  }
}

/** Contains factory methods and implicit conversions for JSON values. */
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
    val parser = new model.JSONReader(string)
    parser.skipWhitespace()
    val jsvalue = parser.parseJSONValue(JSON)
    parser.skipWhitespace()
    parser.parseEnd()
    jsvalue
  }
  
  object unary_+ extends runtime.AbstractPartialFunction[Any, JSValue] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSValue]
    override def apply(x: Any): JSValue = x.asInstanceOf[JSValue]
    override def toString: String = "+JSValue"
  }
}
