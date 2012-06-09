/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

abstract class JSONFactory {
  type JSValue
  
  class JSValueFactory {
    def parse(s: String): JSValue = {
      val parser = JSParser(s)
      parser.parseWhitespace()
      parser.parseJSValue()
    }
  }
  
  val JSValue: JSValueFactory
  
  
  type JSObject <: JSValue
  
  abstract class JSObjectFactory {
    def newBuilder(sizeHint: Int = 16): JSObjectBuilder
    
    def empty: JSObject
    
    def apply(fields: (String, JSValue)*): JSObject
    
    def parse(s: String): JSObject = {
      val parser = JSParser(s)
      parser.parseWhitespace()
      parser.parseJSObject()
    }
  }
  
  abstract class JSObjectBuilder {
    def += (name: String, value: JSValue): Unit
    
    def result: JSObject
  }
  
  val JSObject: JSObjectFactory
  
  
  type JSArray <: JSValue
  
  abstract class JSArrayFactory {
    def newBuilder(sizeHint: Int = 16): JSArrayBuilder
    
    def empty: JSArray
    
    def apply(values: JSValue*): JSArray
    
    def parse(s: String): JSArray = {
      val parser = JSParser(s)
      parser.parseWhitespace()
      parser.parseJSArray()
    }
  }
  
  abstract class JSArrayBuilder {
    def += (json: JSValue): Unit
    
    def result: JSArray
  }
  
  val JSArray: JSArrayFactory
  
  
  type JSString <: JSValue
  
  abstract class JSStringFactory {
    def apply(s: String): JSString
    
    def parse(s: String): JSString = {
      val parser = JSParser(s)
      parser.parseWhitespace()
      parser.parseJSString()
    }
  }
  
  val JSString: JSStringFactory
  
  
  type JSNumber <: JSValue
  
  class JSNumberFactory {
    def parse(s: String): JSNumber = {
      val parser = JSParser(s)
      parser.parseWhitespace()
      parser.parseJSNumber()
    }
  }
  
  val JSNumber: JSNumberFactory
  
  
  type JSInteger <: JSNumber
  
  abstract class JSIntegerFactory {
    def apply(s: String): JSInteger
    
    def apply(n: Int): JSInteger
    
    def apply(n: Long): JSInteger
  }
  
  val JSInteger: JSIntegerFactory
  
  
  type JSDecimal <: JSNumber
  
  abstract class JSDecimalFactory {
    def apply(s: String): JSDecimal
    
    def apply(x: Float): JSDecimal
    
    def apply(x: Double): JSDecimal
  }
  
  val JSDecimal: JSDecimalFactory
  
  
  type JSBoolean <: JSValue
  
  def JSTrue: JSBoolean
  
  def JSFalse: JSBoolean
  
  
  type JSNull <: JSValue
  
  def JSNull: JSNull
  
  
  def JSParser(cs: CharSequence): JSONParser[JSONFactory.this.type] =
    new JSONParser.FromCharSequence(JSONFactory.this, cs)
}
