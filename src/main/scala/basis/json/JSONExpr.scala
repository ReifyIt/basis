/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.Universe

/** A JSON factory that generates expressions that build data structures for
  * another JSON implementation.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> import scala.reflect.mirror; import basis.json._
  * import scala.reflect.mirror
  * import basis.json._
  * 
  * scala> val JSON = new JSONExpr[mirror.type, JSONTree.type](mirror)(mirror.reify(JSONTree))
  * json: basis.json.JSONExpr[reflect.mirror.type,basis.json.JSONTree.type] = JSONExpr
  * 
  * scala> JSON.JSValue.parse("[null]")
  * res0: JSON.JSValue = Expr[Nothing](basis.json.JSONTree.JSArray.newBuilder(1).$plus$eq(basis.json.JSONTree.JSNull).result)
  * }}}
  * 
  * @constructor Constructs a JSON implementation that generates builder expressions
  *              in the given reflection universe for JSON values produced by the
  *              parameterized JSON implementation.
  * @tparam Context   The singleton type of the reflective universe in which to generate expressions.
  * @tparam JSON      The singleton type of the JSON implementation to produce expressions for.
  * @param  context   The reflection universe used to generate expressions.
  * @param  jsonExpr  An expression in `context` that evals to the composed JSON implementation.
  */
class JSONExpr[+Context <: Universe with Singleton, JSON <: JSONContext]
    (val context: Context)(jsonExpr: Context#Expr[JSON])
  extends JSONContext {
  
  import context._
  
  /** An expression that evals to the composed JSON implementation. */
  val json: Expr[JSON] = jsonExpr.asInstanceOf[Expr[JSON]]
  
  
  override type JSValue = Expr[JSON#JSValue]
  
  override val JSValue = new JSValueFactory
  
  
  override type JSObject = Expr[JSON#JSObject]
  
  class JSObjectFactory extends super.JSObjectFactory {
    override def newBuilder(sizeHint: Int): JSONExpr.super.JSObjectBuilder = new JSObjectBuilder(sizeHint)
    
    override def empty: JSObject = Expr(Select(Select(json.tree, newTermName("JSObject")), newTermName("empty")))
    
    override def apply(fields: (String, JSValue)*): JSObject = {
      val newBuilderTree = Apply(Select(Select(json.tree, newTermName("JSObject")), newTermName("newBuilder")),
                                 List(Literal(Constant(fields.length))))
      val buildTree = fields.foldLeft(newBuilderTree) { (builderTree, field) =>
        val (name, valueExpr) = field
        Apply(Select(builderTree, newTermName("$plus$eq")), List(Literal(Constant(name)), valueExpr.tree))
      }
      Expr(Select(buildTree, newTermName("result")))
    }
  }
  
  class JSObjectBuilder(sizeHint: Int) extends super.JSObjectBuilder {
    private[this] val fields = new ArrayBuffer[(String, JSValue)]
    if (sizeHint > 0) fields.sizeHint(sizeHint)
    override def += (name: String, value: JSValue): this.type = { fields += ((name, value)); this }
    override def result: JSObject = if (fields.length != 0) JSObject(fields: _*) else JSObject.empty
  }
  
  override val JSObject = new JSObjectFactory
  
  
  override type JSArray = Expr[JSON#JSArray]
  
  class JSArrayFactory extends super.JSArrayFactory {
    override def newBuilder(sizeHint: Int): JSONExpr.super.JSArrayBuilder = new JSArrayBuilder(sizeHint)
    
    override def empty: JSArray = Expr(Select(Select(json.tree, newTermName("JSArray")), newTermName("empty")))
    
    override def apply(values: JSValue*): JSArray = {
      val newBuilderTree = Apply(Select(Select(json.tree, newTermName("JSArray")), newTermName("newBuilder")),
                                 List(Literal(Constant(values.length))))
      val buildTree = values.foldLeft(newBuilderTree) { (builderTree, valueExpr) =>
        Apply(Select(builderTree, newTermName("$plus$eq")), List(valueExpr.tree))
      }
      Expr(Select(buildTree, newTermName("result")))
    }
  }
  
  class JSArrayBuilder(sizeHint: Int) extends super.JSArrayBuilder {
    private[this] val values = new ArrayBuffer[JSValue]
    if (sizeHint > 0) values.sizeHint(sizeHint)
    override def += (value: JSValue): this.type = { values += value; this }
    override def result: JSArray = if (values.length != 0) JSArray(values: _*) else JSArray.empty
  }
  
  override val JSArray = new JSArrayFactory
  
  
  override type JSString = Expr[JSON#JSString]
  
  class JSStringFactory extends super.JSStringFactory {
    override def apply(s: String): JSString =
      Expr(Apply(Select(json.tree, newTermName("JSString")), List(Literal(Constant(s)))))
  }
  
  override val JSString = new JSStringFactory
  
  
  override type JSNumber = Expr[JSON#JSNumber]
  
  override val JSNumber = new JSNumberFactory
  
  
  override type JSInteger = Expr[JSON#JSInteger]
  
  class JSIntegerFactory extends super.JSIntegerFactory {
    override def apply(s: String): JSInteger = apply(java.lang.Long.valueOf(s))
    
    override def apply(n: Int): JSInteger =
      Expr(Apply(Select(json.tree, newTermName("JSInteger")), List(Literal(Constant(n)))))
    
    override def apply(n: Long): JSInteger =
      Expr(Apply(Select(json.tree, newTermName("JSInteger")), List(Literal(Constant(n)))))
  }
  
  override val JSInteger = new JSIntegerFactory
  
  
  override type JSDecimal = Expr[JSON#JSDecimal]
  
  class JSDecimalFactory extends super.JSDecimalFactory {
    override def apply(s: String): JSDecimal = apply(java.lang.Double.valueOf(s))
    
    override def apply(x: Float): JSDecimal =
      Expr(Apply(Select(json.tree, newTermName("JSDecimal")), List(Literal(Constant(x)))))
    
    override def apply(x: Double): JSDecimal =
      Expr(Apply(Select(json.tree, newTermName("JSDecimal")), List(Literal(Constant(x)))))
  }
  
  override val JSDecimal = new JSDecimalFactory
  
  
  override type JSBoolean = Expr[JSON#JSBoolean]
  
  override def JSTrue: JSBoolean = Expr(Select(json.tree, newTermName("JSTrue")))
  
  override def JSFalse: JSBoolean = Expr(Select(json.tree, newTermName("JSFalse")))
  
  
  override type JSNull = Expr[JSON#JSNull]
  
  override def JSNull: JSNull = Expr(Select(json.tree, newTermName("JSNull")))
  
  override def toString: String = "JSONExpr"
}
