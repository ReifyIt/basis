/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.breakOut
import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.Universe

class JSONExpr[+Context <: Universe with Singleton, JSON <: JSONFactory with Singleton]
    (val context: Context)(jsonExpr: Context#Expr[JSON])
  extends JSONFactory {
  
  import context._
  
  val json: Expr[JSON] = jsonExpr.asInstanceOf[Expr[JSON]]
  
  
  override type JSValue = Expr[JSON#JSValue]
  
  lazy val JSValue = new JSValueFactory
  
  
  override type JSObject = Expr[JSON#JSObject]
  
  class JSObjectFactory extends super.JSObjectFactory {
    override def newBuilder(sizeHint: Int): JSONExpr.super.JSObjectBuilder = new JSObjectBuilder(sizeHint)
    
    override def empty: JSObject = Expr(Select(Select(json.tree, newTermName("JSObject")), newTermName("empty")))
    
    override def apply(fields: (String, JSValue)*): JSObject = {
      val builderDef =
        ValDef(Modifiers(), newTermName("builder"), TypeTree(),
               Apply(Select(Select(json.tree, newTermName("JSObject")), newTermName("newBuilder")),
                     List(Literal(Constant(fields.length)))))
      
      val builderAppends: List[Tree] = fields.map { case (name, expr) =>
        Apply(Select(Ident(newTermName("builder")), newTermName("$plus$eq")),
              List(Literal(Constant(name)), expr.tree))
      } (breakOut)
      
      val builderResult = Select(Ident(newTermName("builder")), newTermName("result"))
      
      Expr(Block(builderDef :: builderAppends, builderResult))
    }
  }
  
  class JSObjectBuilder(sizeHint: Int) extends super.JSObjectBuilder {
    private[this] val fields = new ArrayBuffer[(String, JSValue)]
    if (sizeHint > 0) fields.sizeHint(sizeHint)
    override def += (name: String, value: JSValue): Unit = fields += ((name, value))
    override def result: JSObject = if (fields.length != 0) JSObject(fields: _*) else JSObject.empty
  }
  
  lazy val JSObject = new JSObjectFactory
  
  
  override type JSArray = Expr[JSON#JSArray]
  
  class JSArrayFactory extends super.JSArrayFactory {
    override def newBuilder(sizeHint: Int): JSONExpr.super.JSArrayBuilder = new JSArrayBuilder(sizeHint)
    
    override def empty: JSArray = Expr(Select(Select(json.tree, newTermName("JSArray")), newTermName("empty")))
    
    override def apply(values: JSValue*): JSArray = {
      val builderDef =
        ValDef(Modifiers(), newTermName("builder"), TypeTree(),
               Apply(Select(Select(json.tree, newTermName("JSArray")), newTermName("newBuilder")),
                     List(Literal(Constant(values.length)))))
      
      val builderAppends: List[Tree] = values.map { expr =>
        Apply(Select(Ident(newTermName("builder")), newTermName("$plus$eq")), List(expr.tree))
      } (breakOut)
      
      val builderResult = Select(Ident(newTermName("builder")), newTermName("result"))
      
      Expr(Block(builderDef :: builderAppends, builderResult))
    }
  }
  
  class JSArrayBuilder(sizeHint: Int) extends super.JSArrayBuilder {
    private[this] val values = new ArrayBuffer[JSValue]
    if (sizeHint > 0) values.sizeHint(sizeHint)
    override def += (value: JSValue): Unit = values += value
    override def result: JSArray = if (values.length != 0) JSArray(values: _*) else JSArray.empty
  }
  
  lazy val JSArray = new JSArrayFactory
  
  
  override type JSString = Expr[JSON#JSString]
  
  class JSStringFactory extends super.JSStringFactory {
    override def apply(s: String): JSString =
      Expr(Apply(Select(json.tree, newTermName("JSString")), List(Literal(Constant(s)))))
  }
  
  lazy val JSString = new JSStringFactory
  
  
  override type JSNumber = Expr[JSON#JSNumber]
  
  lazy val JSNumber = new JSNumberFactory
  
  
  override type JSInteger = Expr[JSON#JSInteger]
  
  class JSIntegerFactory extends super.JSIntegerFactory {
    override def apply(s: String): JSInteger = apply(java.lang.Long.valueOf(s))
    
    override def apply(n: Int): JSInteger =
      Expr(Apply(Select(json.tree, newTermName("JSInteger")), List(Literal(Constant(n)))))
    
    override def apply(n: Long): JSInteger =
      Expr(Apply(Select(json.tree, newTermName("JSInteger")), List(Literal(Constant(n)))))
  }
  
  lazy val JSInteger = new JSIntegerFactory
  
  
  override type JSDecimal = Expr[JSON#JSDecimal]
  
  class JSDecimalFactory extends super.JSDecimalFactory {
    override def apply(s: String): JSDecimal = apply(java.lang.Double.valueOf(s))
    
    override def apply(x: Float): JSDecimal =
      Expr(Apply(Select(json.tree, newTermName("JSDecimal")), List(Literal(Constant(x)))))
    
    override def apply(x: Double): JSDecimal =
      Expr(Apply(Select(json.tree, newTermName("JSDecimal")), List(Literal(Constant(x)))))
  }
  
  lazy val JSDecimal = new JSDecimalFactory
  
  
  override type JSBoolean = Expr[JSON#JSBoolean]
  
  override def JSTrue: JSBoolean = Expr(Select(json.tree, newTermName("JSTrue")))
  
  override def JSFalse: JSBoolean = Expr(Select(json.tree, newTermName("JSFalse")))
  
  
  override type JSNull = Expr[JSON#JSNull]
  
  override def JSNull: JSNull = Expr(Select(json.tree, newTermName("JSNull")))
}
