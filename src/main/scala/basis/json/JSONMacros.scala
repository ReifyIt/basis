/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
import basis.json

import scala.collection.mutable.Builder
import scala.reflect.makro.Context

import language.experimental.macros

object JSONMacros {
  def buildJSON(c: Context)(args: c.Expr[JSValue]*): c.Expr[JSValue] = {
    import c.universe._
    val Select(Apply(_, List(Apply(_, literalParts))), _) = c.prefix.tree
    val parts = literalParts map { case Literal(Constant(part: String)) => part }
    
    val macros = new JSONMacros[c.type](c)
    val builder = new macros.BuildExpr
    val interpolator = new JSONInterpolator[builder.type](parts, args)
    
    interpolator.skipWhitespace()
    val buildExpr = interpolator.parseJSValue[builder.type](builder)
    interpolator.skipWhitespace()
    interpolator.parseEnd()
    buildExpr
  }
  
  def matchJSON(c: Context)(jsvalue: c.Expr[JSValue]): c.Expr[Option[Seq[JSValue]]] = {
    import c.universe.{Expr => _, reify => _, _}
    import c._
    val Select(Apply(_, List(Apply(_, literalParts))), _) = prefix.tree
    val parts = literalParts map { case Literal(Constant(part: String)) => part }
    
    val bindingsTermName = fresh(newTermName("bindings"))
    val bindingsValueExpr = reify(Seq.newBuilder[JSValue])
    val bindingsDefExpr = Expr(ValDef(Modifiers(), bindingsTermName, TypeTree(), bindingsValueExpr.tree))
    val bindingsExpr = Expr[Builder[JSValue, Seq[JSValue]]](Ident(bindingsTermName))
    
    val macros = new JSONMacros[c.type](c)
    val matcher = new macros.MatchExpr(jsvalue)
    val parser = new macros.MatchParser(parts, bindingsExpr)
    
    parser.skipWhitespace()
    val matchExpr = parser.parseJSValue(matcher)
    parser.skipWhitespace()
    parser.parseEnd()
    
    reify {
      bindingsDefExpr.splice
      val matchSucceeded = matchExpr.splice
      if (matchSucceeded) Some(bindingsExpr.splice.result) else None
    }
  }
}

class JSONMacros[C <: Context](val context: C) {
  import context.universe.{Expr => _, reify => _, _}
  import context._
  
  class BuildExpr extends JSONContext {
    override type JSValue   = Expr[json.JSValue]
    override type JSObject  = Expr[json.JSObject]
    override type JSArray   = Expr[json.JSArray]
    override type JSString  = Expr[json.JSString]
    override type JSNumber  = Expr[json.JSNumber]
    override type JSInteger = Expr[json.JSInteger]
    override type JSDecimal = Expr[json.JSDecimal]
    override type JSBoolean = Expr[json.JSBoolean]
    override type JSNull    = Expr[json.JSNull]
    
    override def JSObjectBuilder: Builder[(String, Expr[json.JSValue]), Expr[json.JSObject]] = {
      Vector.newBuilder[(String, Expr[json.JSValue])] mapResult { fieldExprs =>
        if (fieldExprs.length == 0) reify(json.JSObject.empty)
        else {
          val lengthExpr = literal(fieldExprs.length)
          val newBuilderExpr = reify(new json.JSObjectBuilder(lengthExpr.splice))
          val builderExpr = fieldExprs.foldLeft(newBuilderExpr) { (expr, fieldExpr) =>
            val (name, valueExpr) = fieldExpr
            val nameExpr = literal(name)
            reify(expr.splice += (nameExpr.splice, valueExpr.splice))
          }
          reify(builderExpr.splice.result)
        }
      }
    }
    
    override def JSArrayBuilder: Builder[Expr[json.JSValue], Expr[json.JSArray]] = {
      Vector.newBuilder[Expr[json.JSValue]] mapResult { valueExprs =>
        if (valueExprs.length == 0) reify(json.JSArray.empty)
        else {
          val lengthExpr = literal(valueExprs.length)
          val newBuilderExpr = reify(new json.JSArrayBuilder(lengthExpr.splice))
          val builderExpr = valueExprs.foldLeft(newBuilderExpr) { (expr, valueExpr) =>
            reify(expr.splice += valueExpr.splice)
          }
          reify(builderExpr.splice.result)
        }
      }
    }
    
    override def JSString(string: String): Expr[json.JSString] = {
      val valueExpr = literal(string)
      reify(new json.JSString(valueExpr.splice))
    }
    
    override def JSInteger(string: String): Expr[json.JSInteger] = {
      val valueExpr = literal(java.lang.Long.valueOf(string))
      reify(new json.JSInteger(valueExpr.splice))
    }
    
    override def JSDecimal(string: String): Expr[json.JSDecimal] = {
      val valueExpr = literal(java.lang.Double.valueOf(string))
      reify(new json.JSDecimal(valueExpr.splice))
    }
    
    override def JSTrue: Expr[json.JSTrue.type] = reify(json.JSTrue)
    
    override def JSFalse: Expr[json.JSFalse.type] = reify(json.JSFalse)
    
    override def JSNull: Expr[json.JSNull.type] = reify(json.JSNull)
  }
  
  
  class MatchExpr(val targetExpr: Expr[json.JSValue]) extends JSONContext {
    override type JSValue   = Expr[Boolean]
    override type JSObject  = Expr[Boolean]
    override type JSArray   = Expr[Boolean]
    override type JSString  = Expr[Boolean]
    override type JSNumber  = Expr[Boolean]
    override type JSInteger = Expr[Boolean]
    override type JSDecimal = Expr[Boolean]
    override type JSBoolean = Expr[Boolean]
    override type JSNull    = Expr[Boolean]
    
    override def JSObjectBuilder: Builder[(String, Expr[Boolean]), Expr[Boolean]] = {
      Vector.newBuilder[(String, Expr[Boolean])] mapResult { fieldExprs =>
        if (fieldExprs.length == 0) literalTrue
        else {
          val (names, valueExprs) = fieldExprs.unzip
          valueExprs.reduceLeft { (expr, valueExpr) =>
            reify(expr.splice && valueExpr.splice)
          }
        }
      }
    }
    
    override def JSArrayBuilder: Builder[Expr[Boolean], Expr[Boolean]] = {
      Vector.newBuilder[Expr[Boolean]] mapResult { valueExprs =>
        val arrayExpr = targetExpr.asInstanceOf[Expr[json.JSArray]]
        if (valueExprs.length == 0) reify(arrayExpr.splice.length == 0)
        else {
          val lengthExpr = literal(valueExprs.length)
          val lengthTestExpr = reify(arrayExpr.splice.length == lengthExpr.splice)
          valueExprs.foldLeft(lengthTestExpr) { (expr, valueExpr) =>
            reify(expr.splice && valueExpr.splice)
          }
        }
      }
    }
    
    override def JSString(string: String): Expr[Boolean] = {
      val valueExpr = literal(string)
      reify(
        targetExpr.splice.isInstanceOf[json.JSString] &&
          targetExpr.splice.asInstanceOf[json.JSString].value == valueExpr.splice
      )
    }
    
    override def JSInteger(string: String): Expr[Boolean] = {
      val valueExpr = literal(java.lang.Long.valueOf(string))
      reify(
        targetExpr.splice.isInstanceOf[json.JSInteger] &&
          targetExpr.splice.asInstanceOf[json.JSInteger].toLong == valueExpr.splice
      )
    }
    
    override def JSDecimal(string: String): Expr[Boolean] = {
      val valueExpr = literal(java.lang.Double.valueOf(string))
      reify(
        targetExpr.splice.isInstanceOf[json.JSNumber] &&
          targetExpr.splice.asInstanceOf[json.JSNumber].toDouble == valueExpr.splice
      )
    }
    
    override def JSTrue: Expr[Boolean] = reify(targetExpr.splice eq json.JSTrue)
    
    override def JSFalse: Expr[Boolean] = reify(targetExpr.splice eq json.JSFalse)
    
    override def JSNull: Expr[Boolean] = reify(targetExpr.splice eq json.JSNull)
  }
  
  
  class MatchParser(parts: Seq[String], val bindingsBuilderExpr: Expr[Builder[JSValue, _]])
    extends JSONPartParser[MatchExpr](parts) {
    
    override protected def readJSValue[T <: MatchExpr](target: T): Expr[Boolean] = {
      if (lookahead == '\u001A') readChar()
      else syntaxError("Expected interpolated value")
      
      val targetExpr = target.targetExpr
      reify {
        bindingsBuilderExpr.splice += targetExpr.splice
        true
      }
    }
    
    override def parseJSObject[T <: MatchExpr](target: T): Expr[Boolean] = {
      val targetExpr = target.targetExpr
      val newTargetTermName = fresh(newTermName("target"))
      val newTargetValueExpr = reify(targetExpr.splice.asInstanceOf[JSObject])
      val newTargetDefExpr = Expr(ValDef(Modifiers(), newTargetTermName, TypeTree(), newTargetValueExpr.tree))
      val newTargetExpr = Expr[JSObject](Ident(newTargetTermName))
      val matcher = new MatchExpr(newTargetExpr)
      val matchExpr = super.parseJSObject(matcher)
      reify(
        targetExpr.splice.isInstanceOf[JSObject] && {
          newTargetDefExpr.splice
          matchExpr.splice
        }
      )
    }
    
    protected override def parseJSFieldValue[T <: MatchExpr](target: T, name: String): Expr[Boolean] = {
      val targetExpr = target.targetExpr.asInstanceOf[Expr[JSObject]]
      val nameExpr = literal(name)
      val newTargetTermName = fresh(newTermName("target"))
      val newTargetValueExpr = reify(targetExpr.splice.apply(nameExpr.splice))
      val newTargetDefExpr = Expr(ValDef(Modifiers(), newTargetTermName, TypeTree(), newTargetValueExpr.tree))
      val newTargetExpr = Expr[JSValue](Ident(newTargetTermName))
      val matcher = new MatchExpr(newTargetExpr)
      val matchExpr = super.parseJSFieldValue(matcher, name)
      reify {
        newTargetDefExpr.splice
        newTargetExpr.splice != JSUndefined && matchExpr.splice
      }
    }
    
    override def parseJSArray[T <: MatchExpr](target: T): Expr[Boolean] = {
      val targetExpr = target.targetExpr
      val newTargetTermName = fresh(newTermName("target"))
      val newTargetValueExpr = reify(targetExpr.splice.asInstanceOf[JSArray])
      val newTargetDefExpr = Expr(ValDef(Modifiers(), newTargetTermName, TypeTree(), newTargetValueExpr.tree))
      val newTargetExpr = Expr[JSArray](Ident(newTargetTermName))
      val matcher = new MatchExpr(newTargetExpr)
      val matchExpr = super.parseJSArray(matcher)
      reify(
        targetExpr.splice.isInstanceOf[JSArray] && {
          newTargetDefExpr.splice
          matchExpr.splice
        }
      )
    }
    
    protected override def parseJSArrayValue[T <: MatchExpr](target: T, index: Int): Expr[Boolean] = {
      val targetExpr = target.targetExpr.asInstanceOf[Expr[JSArray]]
      val indexExpr = literal(index)
      val newTargetTermName = fresh(newTermName("target"))
      val newTargetValueExpr = reify(targetExpr.splice.apply(indexExpr.splice))
      val newTargetDefExpr = Expr(ValDef(Modifiers(), newTargetTermName, TypeTree(), newTargetValueExpr.tree))
      val newTargetExpr = Expr[JSValue](Ident(newTargetTermName))
      val matcher = new MatchExpr(newTargetExpr)
      val matchExpr = super.parseJSArrayValue(matcher, index)
      reify {
        newTargetDefExpr.splice
        matchExpr.splice
      }
    }
  }
}
