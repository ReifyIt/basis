/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import language.experimental.macros

object JSONType extends JSONFactory {
  override type JSValue = Any
  
  lazy val JSValue = new JSValueFactory
  
  
  override type JSObject = Map[String, Any]
  
  object JSObject extends JSObjectFactory {
    override def newBuilder(sizeHint: Int): JSObjectBuilder = new JSObjectBuilder(sizeHint)
    override lazy val empty: Map[String, Any] = Map.empty
    override def apply(fields: (String, Any)*): Map[String, Any] = Map(fields: _*)
  }
  
  final class JSObjectBuilder(sizeHint: Int) extends super.JSObjectBuilder {
    private[this] val fields = Map.newBuilder[String, Any]
    if (sizeHint > 0) fields.sizeHint(sizeHint)
    override def += (name: String, value: Any): Unit = fields += ((name, value))
    override def result: Map[String, Any] = fields.result
  }
  
  
  override type JSArray = Seq[Any]
  
  object JSArray extends JSArrayFactory {
    override def newBuilder(sizeHint: Int): JSArrayBuilder = new JSArrayBuilder(sizeHint)
    override lazy val empty: Seq[Any] = Vector.empty
    override def apply(values: Any*): Seq[Any] = Vector(values: _*)
  }
  
  final class JSArrayBuilder(sizeHint: Int) extends super.JSArrayBuilder {
    private[this] val values = Vector.newBuilder[Any]
    if (sizeHint > 0) values.sizeHint(sizeHint)
    override def += (value: Any): Unit = values += value
    override def result: Seq[Any] = values.result
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
  
  
  implicit class JSStringContext(context: StringContext) {
    def json(args: Any*): Any = macro JSStaticParser.parseJSValue
    def jsobject(args: Any*): Map[String, Any] = macro JSStaticParser.parseJSObject
    def jsarray(args: Any*): Seq[Any] = macro JSStaticParser.parseJSArray
  }
  
  object JSStaticParser {
    import scala.reflect.makro.Context
    
    def newPrefixParser(c: Context)(args: Seq[c.Expr[Any]])
      : JSONParser[JSONExpr[c.mirror.type, JSONType.type] with Singleton] = {
      import c.mirror._
      val Apply(_, List(Apply(_, literals))) = c.prefix.tree
      val parts = literals map { case Literal(Constant(part: String)) => part }
      val jsonExpr = new JSONExpr[c.mirror.type, JSONType.type](c.mirror)(c.reify(JSONType))
      new JSONParser.Interpolating[jsonExpr.type](jsonExpr, parts)(args)
    }
    
    def parseJSValue(c: Context)(args: c.Expr[Any]*): c.Expr[Any] = {
      val parser = newPrefixParser(c)(args)
      parser.parseWhitespace()
      parser.parseJSValue()
    }
    
    def parseJSObject(c: Context)(args: c.Expr[Any]*): c.Expr[Map[String, Any]] = {
      val parser = newPrefixParser(c)(args)
      parser.parseWhitespace()
      parser.parseJSObject()
    }
    
    def parseJSArray(c: Context)(args: c.Expr[Any]*): c.Expr[Seq[Any]] = {
      val parser = newPrefixParser(c)(args)
      parser.parseWhitespace()
      parser.parseJSArray()
    }
  }
}
