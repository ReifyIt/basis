/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.ArrayBuffer
import scala.reflect.makro.Context

import basis.json.model._

private[json] object JSBuilderMacro {
  import language.experimental.macros
  
  def interpolate(c: Context)(args: c.Expr[JSValue]*): c.Expr[JSValue] = {
    import c.universe._
    val Select(Apply(_, List(Apply(_, literalParts))), _) = c.prefix.tree
    val parts = literalParts map { case Literal(Constant(part: String)) => part }
    
    val builder = new JSBuilderMacro[c.type](c)
    val model = new builder.Model
    val parser = new builder.Interpolator(parts, args)
    
    try {
      parser.skipWhitespace()
      val buildExpr = parser.parseJSONValue(model)
      parser.skipWhitespace()
      parser.parseEnd()
      buildExpr
    }
    catch {
      case e: JSONException => c.abort(c.enclosingPosition, e.getMessage)
    }
  }
}

private[json] class JSBuilderMacro[C <: Context](val context: C) {
  import context.universe.{Expr => _, reify => _, _}
  import context._
  
  class Model extends JSONModel {
    override type Value   = Expr[JSValue]
    override type Object  = Expr[JSObject]
    override type Array   = Expr[JSArray]
    override type String  = Expr[JSString]
    override type Number  = Expr[JSNumber]
    override type Integer = Expr[JSInteger]
    override type Decimal = Expr[JSDecimal]
    override type Boolean = Expr[JSBoolean]
    override type Null    = Expr[JSNull.type]
    
    override def ObjectBuilder: ObjectBuilder = new ObjectBuilder
    
    override def ArrayBuilder: ArrayBuilder = new ArrayBuilder
    
    override def String(string: java.lang.String): Expr[JSString] = {
      val valueExpr = literal(string)
      reify(new JSString(valueExpr.splice))
    }
    
    override def Integer(string: java.lang.String): Expr[JSInteger] = {
      val valueExpr = literal(java.lang.Long.valueOf(string))
      reify(new JSInteger(valueExpr.splice))
    }
    
    override def Decimal(string: java.lang.String): Expr[JSDecimal] = {
      val valueExpr = literal(java.lang.Double.valueOf(string))
      reify(new JSDecimal(valueExpr.splice))
    }
    
    override def True: Expr[JSTrue.type] = reify(JSTrue)
    
    override def False: Expr[JSFalse.type] = reify(JSFalse)
    
    override def Null: Expr[JSNull.type] = reify(JSNull)
  }
  
  class ObjectBuilder extends Model with JSONObjectBuilder[Expr[JSObject]] {
    private[this] val fieldExprs = new ArrayBuffer[(java.lang.String, Expr[JSValue])]
    
    override def += (name: java.lang.String, valueExpr: Expr[JSValue]): this.type = {
      fieldExprs += name -> valueExpr
      this
    }
    
    override def result: Expr[JSObject] = {
      if (fieldExprs.length == 0) reify(JSObject.empty)
      else {
        val lengthExpr = literal(fieldExprs.length)
        val builderExpr = reify(new JSObjectBuilder(lengthExpr.splice))
        val buildExpr = fieldExprs.foldLeft(builderExpr) { (expr, fieldExpr) =>
          val (name, valueExpr) = fieldExpr
          val nameExpr = literal(name)
          reify(expr.splice += (nameExpr.splice, valueExpr.splice))
        }
        reify(buildExpr.splice.result)
      }
    }
  }
  
  class ArrayBuilder extends Model with JSONArrayBuilder[Expr[JSArray]] {
    private[this] val valueExprs = new ArrayBuffer[Expr[JSValue]]
    
    override def += (valueExpr: Expr[JSValue]): this.type = {
      valueExprs += valueExpr
      this
    }
    
    override def result: Expr[JSArray] = {
      if (valueExprs.length == 0) reify(JSArray.empty)
      else {
        val lengthExpr = literal(valueExprs.length)
        val builderExpr = reify(new JSArrayBuilder(lengthExpr.splice))
        val buildExpr = valueExprs.foldLeft(builderExpr) { (expr, valueExpr) =>
          reify(expr.splice += valueExpr.splice)
        }
        reify(buildExpr.splice.result)
      }
    }
  }
  
  class Interpolator(parts: Seq[String], args: Seq[Expr[JSValue]]) extends JSONJoiner(parts) {
    private[this] var nextArgIndex: Int = 0
    
    override protected def readJSONValue(model: JSONModel): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      val arg = args(nextArgIndex)
      nextArgIndex += 1
      arg.asInstanceOf[model.Value]
    }
    
    override protected def parseJSONName(): String = {
      if (lookahead == 0x1A) {
        val arg = args(nextArgIndex)
        if (!arg.isInstanceOf[JSString]) syntaxError("interpolated field name is not a JSString")
        readChar()
        nextArgIndex += 1
        arg.asInstanceOf[JSString].value
      }
      else super.parseJSONName()
    }
  }
}
