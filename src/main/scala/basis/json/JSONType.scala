/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.Builder

import language.experimental.macros

/** A JSON model implemented by general Scala types and supporting compile-time
  * string interpolation.
  * 
  * ==Data types==
  * 
  * This implementation maps JSON values to the following:
  * 
  *   - `JSValue`   ⇒ `scala.Any`
  *   - `JSObject`  ⇒ `scala.collection.Map[java.lang.String, scala.Any]`
  *   - `JSArray`   ⇒ `scala.collection.Seq[scala.Any]`
  *   - `JSString`  ⇒ `java.lang.String`
  *   - `JSNumber`  ⇒ `scala.Any`
  *   - `JSInteger` ⇒ `scala.Long`
  *   - `JSDecimal` ⇒ `scala.Double`
  *   - `JSBoolean` ⇒ `scala.Boolean`
  *   - `JSNull`    ⇒ `scala.Null`
  * 
  * ==String interpolation==
  * 
  * The `JSStringContext` class provides these string interpolation macros:
  * 
  *   - `json""` – statically parses any JSON value.
  *   - `jsobject""` – statically parses a JSON object.
  *   - `jsarray""` – statically parses a JSON array.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> import basis.json.JSONType._
  * import basis.json.JSONType._
  * 
  * scala> json""" [{}, [], "", 0, 0.0, true, false, null] """
  * res0: Seq[Any] = Vector(Map(), Vector(), "", 0, 0.0, true, false, null)
  * 
  * scala> jsarray"[0,1,2,3,4]" map { case n: Long => math.pow(2, n).toLong }
  * res1: Seq[Long] = Vector(1, 2, 4, 8, 16)
  * 
  * scala> ((n: Int, s: String) => jsarray"[$n, $s]")(0, "zero")
  * res2: Seq[Any] = Vector(0, zero)
  * }}}
  */
object JSONType extends JSONContext {
  override type JSValue = Any
  
  lazy val JSValue = new JSValueFactory
  
  
  override type JSObject = Map[String, Any]
  
  object JSObject extends JSObjectFactory {
    override def newBuilder(sizeHint: Int = 0): JSObjectBuilder = new JSObjectBuilder(sizeHint)
    override lazy val empty: Map[String, Any] = Map.empty
    override def apply(fields: (String, Any)*): Map[String, Any] = Map(fields: _*)
  }
  
  final class JSObjectBuilder(sizeHint: Int) extends super.JSObjectBuilder with Builder[(String, Any), JSObject] {
    private[this] var builder = Map.newBuilder[String, Any]
    if (sizeHint > 0) builder.sizeHint(sizeHint)
    override def sizeHint(size: Int): Unit = builder.sizeHint(size)
    override def += (name: String, value: Any): this.type = { builder += ((name, value)); this }
    override def += (field: (String, Any)): this.type = { builder += field; this }
    override def result: Map[String, Any] = builder.result
    override def clear(): Unit = builder = Map.newBuilder[String, Any]
  }
  
  
  override type JSArray = Seq[Any]
  
  object JSArray extends JSArrayFactory {
    override def newBuilder(sizeHint: Int = 0): JSArrayBuilder = new JSArrayBuilder(sizeHint)
    override lazy val empty: Seq[Any] = Vector.empty
    override def apply(values: Any*): Seq[Any] = Vector(values: _*)
  }
  
  final class JSArrayBuilder(sizeHint: Int) extends super.JSArrayBuilder with Builder[Any, JSArray] {
    private[this] var builder = Vector.newBuilder[Any]
    if (sizeHint > 0) builder.sizeHint(sizeHint)
    override def sizeHint(size: Int): Unit = builder.sizeHint(size)
    override def += (value: Any): this.type = { builder += value; this }
    override def result: Seq[Any] = builder.result
    override def clear(): Unit = builder = Vector.newBuilder[Any]
  }
  
  
  override type JSString = String
  
  object JSString extends JSStringFactory {
    override def apply(s: String): String = s
  }
  
  
  override type JSNumber = Any
  
  lazy val JSNumber = new JSNumberFactory
  
  
  override type JSInteger = Long
  
  object JSInteger extends JSIntegerFactory {
    override def apply(s: String): Long = java.lang.Long.valueOf(s)
    override def apply(n: Int): Long = n
    override def apply(n: Long): Long = n
  }
  
  
  override type JSDecimal = Double
  
  object JSDecimal extends JSDecimalFactory {
    override def apply(s: String): Double = java.lang.Double.valueOf(s)
    override def apply(x: Float): Double = x
    override def apply(x: Double): Double = x
  }
  
  
  override type JSBoolean = Boolean
  
  override def JSTrue: Boolean = true
  override def JSFalse: Boolean = false
  
  
  override type JSNull = Null
  
  override def JSNull: Null = null
  
  
  /** Provides `json`, `jsobject`, and `jsarray` string interpolators. */
  class JSStringContext(sc: StringContext) {
    def json(args: JSValue*): JSValue = macro JSStringContext.json
    
    def jsobject(args: JSValue*): JSObject = macro JSStringContext.jsobject
    
    def jsarray(args: JSValue*): JSArray = macro JSStringContext.jsarray
  }
  
  /** Contains string interpolation macro implementations. */
  implicit object JSStringContext extends (StringContext => JSStringContext) {
    import scala.reflect.makro.Context
    
    override def apply(sc: StringContext) = new JSStringContext(sc)
    
    def json(context: Context)(args: context.Expr[JSValue]*): context.Expr[JSValue] =
      JSONParser.StaticInterpolator.parseJSValue[JSONType.type](context)(context.reify(JSONType), args)
    
    def jsobject(context: Context)(args: context.Expr[JSValue]*): context.Expr[JSObject] =
      JSONParser.StaticInterpolator.parseJSObject[JSONType.type](context)(context.reify(JSONType), args)
    
    def jsarray(context: Context)(args: context.Expr[JSValue]*): context.Expr[JSArray] =
      JSONParser.StaticInterpolator.parseJSArray[JSONType.type](context)(context.reify(JSONType), args)
  }
}
