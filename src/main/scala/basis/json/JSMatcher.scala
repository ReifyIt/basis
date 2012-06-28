/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Builder

import basis.json.model._

private[json] object JSMatcher {
  def interpolate(jsvalue: JSValue, parts: Seq[String]): Option[Seq[JSValue]] = {
    val bindings = new ArrayBuffer[JSValue]
    val model = new Model(jsvalue)
    val parser = new Interpolator(parts, bindings)
    
    parser.skipWhitespace()
    val matches = parser.parseJSONValue(model)
    parser.skipWhitespace()
    parser.parseEnd()
    
    if (matches) Some(bindings) else None
  }
  
  class Model(val jsvalue: JSValue) extends JSONModel {
    override type Value   = scala.Boolean
    override type Object  = scala.Boolean
    override type Array   = scala.Boolean
    override type String  = scala.Boolean
    override type Number  = scala.Boolean
    override type Integer = scala.Boolean
    override type Decimal = scala.Boolean
    override type Boolean = scala.Boolean
    override type Null    = scala.Boolean
    
    override def ObjectBuilder: ObjectBuilder = new ObjectBuilder(jsvalue)
    
    override def ArrayBuilder: ArrayBuilder = new ArrayBuilder(jsvalue)
    
    override def String(value: java.lang.String): scala.Boolean =
      jsvalue.isInstanceOf[JSString] &&
      jsvalue.asInstanceOf[JSString].value == value
    
    override def Integer(value: java.lang.String): scala.Boolean =
      jsvalue.isInstanceOf[JSInteger] &&
      jsvalue.asInstanceOf[JSInteger].toLong == java.lang.Long.valueOf(value)
    
    override def Decimal(value: java.lang.String): scala.Boolean =
      jsvalue.isInstanceOf[JSDecimal] &&
      jsvalue.asInstanceOf[JSDecimal].toDouble == java.lang.Double.valueOf(value)
    
    override def True: scala.Boolean = jsvalue eq JSTrue
    
    override def False: scala.Boolean = jsvalue eq JSFalse
    
    override def Null: scala.Boolean = jsvalue eq JSNull
  }
  
  class ObjectBuilder(override val jsvalue: JSValue)
    extends Model(jsvalue) with JSONObjectBuilder[scala.Boolean] {
    
    private[this] var matches: Boolean = jsvalue.isInstanceOf[JSObject]
    
    private[this] def jsobject = jsvalue.asInstanceOf[JSObject]
    
    override def += (name: java.lang.String, value: scala.Boolean): this.type = {
      matches = matches && value
      this
    }
    
    override def result: scala.Boolean = matches
    
    def ValueForName(name: java.lang.String): JSValue =
      if (matches) jsobject(name) else JSUndefined
    
    override def ObjectBuilderForName(name: java.lang.String): ObjectBuilder =
      new ObjectBuilder(if (matches) jsobject(name) else JSUndefined)
    
    override def ArrayBuilderForName(name: java.lang.String): ArrayBuilder =
      new ArrayBuilder(if (matches) jsobject(name) else JSUndefined)
    
    override def StringForName(name: java.lang.String, value: java.lang.String): scala.Boolean = {
      val jsitem = jsobject(name)
      jsitem.isInstanceOf[JSString] && jsitem.asInstanceOf[JSString].value == value
    }
    
    override def IntegerForName(name: java.lang.String, value: java.lang.String): scala.Boolean = {
      val jsitem = jsobject(name)
      jsitem.isInstanceOf[JSInteger] && jsitem.asInstanceOf[JSInteger].toLong == java.lang.Long.valueOf(value)
    }
    
    override def DecimalForName(name: java.lang.String, value: java.lang.String): scala.Boolean = {
      val jsitem = jsobject(name)
      jsitem.isInstanceOf[JSDecimal] && jsitem.asInstanceOf[JSDecimal].toDouble == java.lang.Double.valueOf(value)
    }
    
    override def TrueForName(name: java.lang.String): scala.Boolean =
      matches && (jsobject(name) eq JSTrue)
    
    override def FalseForName(name: java.lang.String): scala.Boolean =
      matches && (jsobject(name) eq JSFalse)
    
    override def NullForName(name: java.lang.String): scala.Boolean =
      matches && (jsobject(name) eq JSNull)
  }
  
  class ArrayBuilder(override val jsvalue: JSValue)
    extends Model(jsvalue) with JSONArrayBuilder[scala.Boolean] {
    
    private[this] var length: Int = 0
    private[this] var matches: Boolean = jsvalue.isInstanceOf[JSArray]
    
    private[this] def jsarray = jsvalue.asInstanceOf[JSArray]
    
    override def += (value: scala.Boolean): this.type = {
      matches = matches && value
      length += 1
      this
    }
    
    override def result: scala.Boolean = matches && jsarray.length == length
    
    def ValueForIndex(index: Int): JSValue =
      if (matches && index < jsarray.length) jsarray(index) else JSUndefined
    
    override def ObjectBuilderForIndex(index: Int): ObjectBuilder =
      new ObjectBuilder(if (matches && index < jsarray.length) jsarray(index) else JSUndefined)
    
    override def ArrayBuilderForIndex(index: Int): ArrayBuilder =
      new ArrayBuilder(if (matches && index < jsarray.length) jsarray(index) else JSUndefined)
    
    override def StringForIndex(index: Int, value: java.lang.String): scala.Boolean = {
      matches && index < jsarray.length && {
        val jsitem = jsarray(index)
        jsitem.isInstanceOf[JSString] && jsitem.asInstanceOf[JSString].value == value
      }
    }
    
    override def IntegerForIndex(index: Int, value: java.lang.String): scala.Boolean = {
      matches && index < jsarray.length && {
        val jsitem = jsarray(index)
        jsitem.isInstanceOf[JSInteger] && jsitem.asInstanceOf[JSInteger].toLong == java.lang.Long.valueOf(value)
      }
    }
    
    override def DecimalForIndex(index: Int, value: java.lang.String): scala.Boolean = {
      matches && index < jsarray.length && {
        val jsitem = jsarray(index)
        jsitem.isInstanceOf[JSDecimal] && jsitem.asInstanceOf[JSDecimal].toDouble == java.lang.Double.valueOf(value)
      }
    }
    
    override def TrueForIndex(index: Int): scala.Boolean =
      matches && index < jsarray.length && (jsarray(index) eq JSTrue)
    
    override def FalseForIndex(index: Int): scala.Boolean =
      matches && index < jsarray.length && (jsarray(index) eq JSFalse)
    
    override def NullForIndex(index: Int): scala.Boolean =
      matches && index < jsarray.length && (jsarray(index) eq JSNull)
  }
  
  class Interpolator(parts: Seq[String], bindings: Builder[JSValue, _]) extends JSONJoiner(parts) {
    override protected def readJSONValue(model: JSONModel): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      val jsvalue = model.asInstanceOf[Model].jsvalue
      bindings += jsvalue
      (jsvalue ne JSUndefined).asInstanceOf[model.Value]
    }
    
    override protected def readJSONValueWithName(model: JSONModel, name: String): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      val jsvalue = model.asInstanceOf[ObjectBuilder].ValueForName(name)
      bindings += jsvalue
      (jsvalue ne JSUndefined).asInstanceOf[model.Value]
    }
    
    override protected def readJSONValueWithIndex(model: JSONModel, index: Int): model.Value = {
      if (lookahead == 0x1A) readChar() else syntaxError(specifyError("expected interpolated value"))
      val jsvalue = model.asInstanceOf[ArrayBuilder].ValueForIndex(index)
      bindings += jsvalue
      (jsvalue ne JSUndefined).asInstanceOf[model.Value]
    }
  }
}
