/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Builder
import scala.reflect.makro.Context

import basis.json.model._

private[json] object JSMatcherMacro {
  import language.experimental.macros
  
  def interpolate(c: Context)(jsvalue: c.Expr[JSValue]): c.Expr[Option[Seq[JSValue]]] = {
    import c.universe.{Expr => _, reify => _, _}
    import c._
    val Select(Apply(_, List(Apply(_, literalParts))), _) = prefix.tree
    val parts = literalParts map { case Literal(Constant(part: String)) => part }
    
    val bindingsExpr = Expr[Builder[JSValue, Seq[JSValue]]](Ident(newTermName("bindings")))
    
    val matcher = new JSMatcherMacro[c.type](c)
    val model = new matcher.Model(jsvalue)
    val parser = new matcher.Interpolator(parts, bindingsExpr)
    
    val matchExpr = try {
      parser.skipWhitespace()
      val matchExpr = parser.parseJSONValue(model)
      parser.skipWhitespace()
      parser.parseEnd()
      matchExpr
    }
    catch {
      case e: JSONException => c.abort(c.enclosingPosition, e.getMessage)
      case e => throw e
    }
    
    reify {
      val bindings = Seq.newBuilder[JSValue]
      if (matchExpr.splice) Some(bindings.result) else None
    }
  }
}

private[json] class JSMatcherMacro[C <: Context](val context: C) {
  import context.universe.{Expr => _, reify => _, _}
  import context._
  
  class Model(val jsvalueExpr: Expr[JSValue]) extends JSONModel {
    override type Value   = Expr[scala.Boolean]
    override type Object  = Expr[scala.Boolean]
    override type Array   = Expr[scala.Boolean]
    override type String  = Expr[scala.Boolean]
    override type Number  = Expr[scala.Boolean]
    override type Integer = Expr[scala.Boolean]
    override type Decimal = Expr[scala.Boolean]
    override type Boolean = Expr[scala.Boolean]
    override type Null    = Expr[scala.Boolean]
    
    override def ObjectBuilder: ObjectBuilder = new ObjectBuilder(jsvalueExpr)
    
    override def ArrayBuilder: ArrayBuilder = new ArrayBuilder(jsvalueExpr)
    
    override def String(value: java.lang.String): Expr[scala.Boolean] = {
      val valueExpr = literal(value)
      reify(jsvalueExpr.splice.isInstanceOf[JSString] &&
            jsvalueExpr.splice.asInstanceOf[JSString].value == valueExpr.splice)
    }
    
    override def Integer(value: java.lang.String): Expr[scala.Boolean] = {
      val valueExpr = literal(java.lang.Long.valueOf(value))
      reify(jsvalueExpr.splice.isInstanceOf[JSInteger] &&
            jsvalueExpr.splice.asInstanceOf[JSInteger].toLong == valueExpr.splice)
    }
    
    override def Decimal(value: java.lang.String): Expr[scala.Boolean] = {
      val valueExpr = literal(java.lang.Double.valueOf(value))
      reify(jsvalueExpr.splice.isInstanceOf[JSDecimal] &&
            jsvalueExpr.splice.asInstanceOf[JSDecimal].toDouble == valueExpr.splice)
    }
    
    override def True: Expr[scala.Boolean] = reify(jsvalueExpr.splice eq JSTrue)
    
    override def False: Expr[scala.Boolean] = reify(jsvalueExpr.splice eq JSFalse)
    
    override def Null: Expr[scala.Boolean] = reify(jsvalueExpr.splice eq JSNull)
  }
  
  class ObjectBuilder(override val jsvalueExpr: Expr[JSValue])
    extends Model(jsvalueExpr) with JSONObjectBuilder[Expr[scala.Boolean]] {
    
    private[this] val fieldExprs = new ArrayBuffer[(java.lang.String, Expr[scala.Boolean])]
    
    def jsobjectExpr = Expr[JSObject](Ident(newTermName("jsobject")))
    
    override def += (name: java.lang.String, valueExpr: Expr[scala.Boolean]): this.type = {
      fieldExprs += name -> valueExpr
      this
    }
    
    override def result: Expr[scala.Boolean] = {
      if (fieldExprs.length == 0) reify(jsvalueExpr.splice.isInstanceOf[JSObject])
      else {
        val (names, valueExprs) = fieldExprs.unzip
        val predicateExpr = valueExprs.reduceLeft { (expr, valueExpr) =>
          reify(expr.splice && valueExpr.splice)
        }
        reify {
          val jsvalue = jsvalueExpr.splice
          jsvalue.isInstanceOf[JSObject] && {
            val jsobject = jsvalue.asInstanceOf[JSObject]
            predicateExpr.splice
          }
        }
      }
    }
    
    override def ObjectBuilderForName(name: java.lang.String): ObjectBuilder = {
      val nameExpr = literal(name)
      new ObjectBuilder(reify(jsobjectExpr.splice(nameExpr.splice)))
    }
    
    override def ArrayBuilderForName(name: java.lang.String): ArrayBuilder = {
      val nameExpr = literal(name)
      new ArrayBuilder(reify(jsobjectExpr.splice(nameExpr.splice)))
    }
    
    override def StringForName(name: java.lang.String, value: java.lang.String): Expr[scala.Boolean] = {
      val nameExpr = literal(name)
      val valueExpr = literal(value)
      reify {
        val jsitem = jsobjectExpr.splice(nameExpr.splice)
        jsitem.isInstanceOf[JSString] && jsitem.asInstanceOf[JSString].value == valueExpr.splice
      }
    }
    
    override def IntegerForName(name: java.lang.String, value: java.lang.String): Expr[scala.Boolean] = {
      val nameExpr = literal(name)
      val valueExpr = literal(java.lang.Long.valueOf(value))
      reify {
        val jsitem = jsobjectExpr.splice(nameExpr.splice)
        jsitem.isInstanceOf[JSInteger] && jsitem.asInstanceOf[JSInteger].toLong == valueExpr.splice
      }
    }
    
    override def DecimalForName(name: java.lang.String, value: java.lang.String): Expr[scala.Boolean] = {
      val nameExpr = literal(name)
      val valueExpr = literal(java.lang.Double.valueOf(value))
      reify {
        val jsitem = jsobjectExpr.splice(nameExpr.splice)
        jsitem.isInstanceOf[JSDecimal] && jsitem.asInstanceOf[JSDecimal].toDouble == valueExpr.splice
      }
    }
    
    override def TrueForName(name: java.lang.String): Expr[scala.Boolean] = {
      val nameExpr = literal(name)
      reify(jsobjectExpr.splice(nameExpr.splice) eq JSTrue)
    }
    
    override def FalseForName(name: java.lang.String): Expr[scala.Boolean] = {
      val nameExpr = literal(name)
      reify(jsobjectExpr.splice(nameExpr.splice) eq JSFalse)
    }
    
    override def NullForName(name: java.lang.String): Expr[scala.Boolean] = {
      val nameExpr = literal(name)
      reify(jsobjectExpr.splice(nameExpr.splice) eq JSNull)
    }
  }
  
  class ArrayBuilder(override val jsvalueExpr: Expr[JSValue])
    extends Model(jsvalueExpr) with JSONArrayBuilder[Expr[scala.Boolean]] {
    
    private[this] val valueExprs = new ArrayBuffer[Expr[scala.Boolean]]
    
    def jsarrayExpr = Expr[JSArray](Ident(newTermName("jsarray")))
    
    override def += (valueExpr: Expr[scala.Boolean]): this.type = {
      valueExprs += valueExpr
      this
    }
    
    override def result: Expr[scala.Boolean] = {
      val lengthExpr = literal(valueExprs.length)
      val lengthTestExpr = reify(jsarrayExpr.splice.length == lengthExpr.splice)
      val predicateExpr = valueExprs.foldLeft(lengthTestExpr) { (expr, valueExpr) =>
        reify(expr.splice && valueExpr.splice)
      }
      reify {
        val jsvalue = jsvalueExpr.splice
        jsvalue.isInstanceOf[JSArray] && {
          val jsarray = jsvalue.asInstanceOf[JSArray]
          predicateExpr.splice
        }
      }
    }
    
    override def ObjectBuilderForIndex(index: Int): ObjectBuilder = {
      val indexExpr = literal(index)
      new ObjectBuilder(reify(jsarrayExpr.splice(indexExpr.splice)))
    }
    
    override def ArrayBuilderForIndex(index: Int): ArrayBuilder = {
      val indexExpr = literal(index)
      new ArrayBuilder(reify(jsarrayExpr.splice(indexExpr.splice)))
    }
    
    override def StringForIndex(index: Int, value: java.lang.String): Expr[scala.Boolean] = {
      val indexExpr = literal(index)
      val valueExpr = literal(value)
      reify {
        val jsitem = jsarrayExpr.splice(indexExpr.splice)
        jsitem.isInstanceOf[JSString] && jsitem.asInstanceOf[JSString].value == valueExpr.splice
      }
    }
    
    override def IntegerForIndex(index: Int, value: java.lang.String): Expr[scala.Boolean] = {
      val indexExpr = literal(index)
      val valueExpr = literal(java.lang.Long.valueOf(value))
      reify {
        val jsitem = jsarrayExpr.splice(indexExpr.splice)
        jsitem.isInstanceOf[JSInteger] && jsitem.asInstanceOf[JSInteger].toLong == valueExpr.splice
      }
    }
    
    override def DecimalForIndex(index: Int, value: java.lang.String): Expr[scala.Boolean] = {
      val indexExpr = literal(index)
      val valueExpr = literal(java.lang.Double.valueOf(value))
      reify {
        val jsitem = jsarrayExpr.splice(indexExpr.splice)
        jsitem.isInstanceOf[JSDecimal] && jsitem.asInstanceOf[JSDecimal].toDouble == valueExpr.splice
      }
    }
    
    override def TrueForIndex(index: Int): Expr[scala.Boolean] = {
      val indexExpr = literal(index)
      reify(jsarrayExpr.splice(indexExpr.splice) eq JSTrue)
    }
    
    override def FalseForIndex(index: Int): Expr[scala.Boolean] = {
      val indexExpr = literal(index)
      reify(jsarrayExpr.splice(indexExpr.splice) eq JSFalse)
    }
    
    override def NullForIndex(index: Int): Expr[scala.Boolean] = {
      val indexExpr = literal(index)
      reify(jsarrayExpr.splice(indexExpr.splice) eq JSNull)
    }
  }
  
  class Interpolator(parts: Seq[String], bindingsExpr: Expr[Builder[JSValue, _]]) extends JSONJoiner(parts) {
    override protected def readJSONValue(model: JSONModel): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      reify {
        bindingsExpr.splice += model.asInstanceOf[Model].jsvalueExpr.splice
        true
      }.asInstanceOf[model.Value]
    }
    
    override protected def readJSONValueWithName(model: JSONModel, name: String): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      val jsobjectExpr = model.asInstanceOf[ObjectBuilder].jsobjectExpr
      val nameExpr = literal(name)
      reify {
        val jsitem = jsobjectExpr.splice(nameExpr.splice)
        (jsitem ne JSUndefined) && {
          bindingsExpr.splice += jsitem
          true
        }
      }.asInstanceOf[model.Value]
    }
    
    override protected def readJSONValueWithIndex(model: JSONModel, index: Int): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      val jsarrayExpr = model.asInstanceOf[ArrayBuilder].jsarrayExpr
      val indexExpr = literal(index)
      reify {
        bindingsExpr.splice += jsarrayExpr.splice(indexExpr.splice)
        true
      }.asInstanceOf[model.Value]
    }
  }
}
