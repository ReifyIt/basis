/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

/** Contains abstract types and factories for JSON values.
  * 
  * @author Chris Sachs
  */
abstract class JSONContext {
  /** The root type of all JSON values. */
  type JSValue
  
  /** A factory for JSON values. */
  class JSValueFactory {
    /** Constructs a JSON value by parsing the given char sequence. */
    def parse(cs: CharSequence): JSValue = {
      val parser = JSParser(cs)
      parser.skipWhitespace()
      parser.parseJSValue(JSONContext.this)
    }
  }
  
  /** Contains factory methods for JSON values. */
  val JSValue: JSValueFactory
  
  
  /** The type of JSON objects. */
  type JSObject <: JSValue
  
  /** An abstract factory for JSON objects. */
  abstract class JSObjectFactory {
    /** Returns a new JSON object builder. */
    def newBuilder(sizeHint: Int = 16): JSObjectBuilder
    
    /** Returns an empty JSON object. */
    def empty: JSObject
    
    /** Returns a JSON object containing the given ''name'', ''value'' pairs. */
    def apply(fields: (String, JSValue)*): JSObject
    
    /** Constructs a JSON object by parsing the given char sequence. */
    def parse(cs: CharSequence): JSObject = {
      val parser = JSParser(cs)
      parser.skipWhitespace()
      parser.parseJSObject(JSONContext.this)
    }
  }
  
  /** A builder for JSON objects. This specialized interface exists to reduce
    * generated code size of interpolated JSON text. */
  abstract class JSObjectBuilder {
    /** Includes a ''name'', ''value'' pair in the constructed JSON object. */
    def += (name: String, value: JSValue): this.type
    
    /** Returns the constructed object and invalidates the builder. */
    def result: JSObject
  }
  
  /** Contains factory methods for JSON objects. */
  val JSObject: JSObjectFactory
  
  
  /** The type of JSON arrays. */
  type JSArray <: JSValue
  
  /** An abstract factory for JSON arrays. */
  abstract class JSArrayFactory {
    /** Returns a new JSON array builder. */
    def newBuilder(sizeHint: Int = 16): JSArrayBuilder
    
    /** Returns an empty JSON array. */
    def empty: JSArray
    
    /** Returns a JSON array containing the given values. */
    def apply(values: JSValue*): JSArray
    
    /** Constructs a JSON array by parsing the given char sequence. */
    def parse(cs: CharSequence): JSArray = {
      val parser = JSParser(cs)
      parser.skipWhitespace()
      parser.parseJSArray(JSONContext.this)
    }
  }
  
  /** A builder for JSON arrays. This specialized interface exists to reduce
    * generated code size of interpolated JSON text. */
  abstract class JSArrayBuilder {
    /** Appends a value to the constructed array. */
    def += (json: JSValue): this.type
    
    /** Returns the constructed array and invalidates the builder. */
    def result: JSArray
  }
  
  /** Contains factory methods for JSON arrays. */
  val JSArray: JSArrayFactory
  
  
  /** The type of JSON string values. */
  type JSString <: JSValue
  
  /** An abstract factory for JSON strings. */
  abstract class JSStringFactory {
    def apply(s: String): JSString
    
    /** Constructs a JSON string by parsing the given char sequence. */
    def parse(cs: CharSequence): JSString = {
      val parser = JSParser(cs)
      parser.skipWhitespace()
      parser.parseJSString(JSONContext.this)
    }
  }
  
  /** Contains factory methods for JSON strings. */
  val JSString: JSStringFactory
  
  
  /** The root type of JSON numbers. */
  type JSNumber <: JSValue
  
  /** An abstract factory for JSON numbers. */
  class JSNumberFactory {
    /** Constructs a JSON number by parsing the given char sequence. */
    def parse(cs: CharSequence): JSNumber = {
      val parser = JSParser(cs)
      parser.skipWhitespace()
      parser.parseJSNumber(JSONContext.this)
    }
  }
  
  /** Contains factory methods for JSON numbers. */
  val JSNumber: JSNumberFactory
  
  
  /** The type of integral JSON numbers. */
  type JSInteger <: JSNumber
  
  /** An abstract factory for integral JSON numbers. */
  abstract class JSIntegerFactory {
    /** Constructs an integral JSON number by parsing the given string. */
    def apply(s: String): JSInteger
    
    /** Returns an integral JSON number representing the given `Int` value. */
    def apply(n: Int): JSInteger
    
    /** Returns an integral JSON number representing the given `Long` value. */
    def apply(n: Long): JSInteger
  }
  
  /** Contains factory methods for integral JSON numbers. */
  val JSInteger: JSIntegerFactory
  
  
  /** The type of decimal JSON numbers. */
  type JSDecimal <: JSNumber
  
  /** An abstract factory for decimal JSON numbers. */
  abstract class JSDecimalFactory {
    /** Constructs a decimal JSON number by parsing the given string. */
    def apply(s: String): JSDecimal
    
    /** Returns a decimal JSON number representing the givem `Float` value. */
    def apply(x: Float): JSDecimal
    
    /** Returns a decimal JSON number representing the given `Double` value. */
    def apply(x: Double): JSDecimal
  }
  
  /** Contains factory methods for decimal JSON numbers. */
  val JSDecimal: JSDecimalFactory
  
  
  /** The type of JSON boolean values. */
  type JSBoolean <: JSValue
  
  /** The `true` JSON boolean value. */
  def JSTrue: JSBoolean
  
  /** The `false` JSON boolean value. */
  def JSFalse: JSBoolean
  
  
  /** The type of the `null` JSON value. */
  type JSNull <: JSValue
  
  /** The `null` JSON value. */
  def JSNull: JSNull
  
  /** Returns a JSON parser for the givene char sequence. */
  protected def JSParser(cs: CharSequence): JSONParser[JSONContext.this.type] =
    new JSONParser.FromCharSequence[JSONContext.this.type](cs)
}
