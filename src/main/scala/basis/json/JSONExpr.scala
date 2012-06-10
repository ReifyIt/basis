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

/** A JSON factory that generates expressions that build data structures for
  * another JSON implementation.
  * 
  * @author Chris Sachs
  * 
  * @note This implementation currently generates expressions non-hygienically.
  *       The typechecker will not warn you if the `JSONFactory` API changes.
  *       Simple reifications fail when splicing the `json` factory expression
  *       (Cannot materialize Expr). And I don't know how to generate the
  *       builder block expressions reifically (awesome, new word!).
  *       Suggestions welcome (send to [[mailto:c9r@me.com Chris Sachs]])!
  * 
  * @example {{{
  * // The expression json""" [{}, [], "", 0, 0.0, true, false, null] """
  * // generates the following code for the givem json implementation:
  * val builder = json.newBuilder(8)
  * builder += json.JSObject.empty
  * builder += json.JSArray.empty
  * builder += json.JSString("")
  * builder += json.JSInteger(0L)
  * builder += json.JSDecimal(0.0)
  * builder += json.JSTrue
  * builder += json.JSFalse
  * builder += json.JSNull
  * builder.result
  * }}}
  * 
  * @constructor Constructs a JSON implementation that generates builder expressions
  *              in the given reflective universe for JSON values produced by the
  *              parameterized JSON implementation.
  * @tparam Context   The singleton type of the reflective universe in which to generate expressions.
  * @tparam JSON      The singleton type of the JSON implementation to produce expressions for.
  * @param  context   The reflective universe used to generate expressions.
  * @param  jsonExpr  An expression in `context` that evals to the composed JSON implementation.
  */
class JSONExpr[+Context <: Universe with Singleton, JSON <: JSONFactory with Singleton]
    (val context: Context)(jsonExpr: Context#Expr[JSON])
  extends JSONFactory {
  
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
  
  override val JSObject = new JSObjectFactory
  
  
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
  
  //override def JSTrue: JSBoolean = reify(json.eval.JSTrue)
  override def JSTrue: JSBoolean = Expr(Select(json.tree, newTermName("JSTrue")))
  
  //override def JSFalse: JSBoolean = reify(json.eval.JSFalse)
  override def JSFalse: JSBoolean = Expr(Select(json.tree, newTermName("JSFalse")))
  
  
  override type JSNull = Expr[JSON#JSNull]
  
  //override def JSNull: JSNull = reify(json.eval.JSNull)
  override def JSNull: JSNull = Expr(Select(json.tree, newTermName("JSNull")))
}
