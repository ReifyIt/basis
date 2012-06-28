/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

class JSON extends model.JSONModel {
  override type Value   = JSValue
  override type Object  = JSObject
  override type Array   = JSArray
  override type String  = JSString
  override type Number  = JSNumber
  override type Integer = JSInteger
  override type Decimal = JSDecimal
  override type Boolean = JSBoolean
  override type Null    = JSNull
  
  override def ObjectBuilder: JSObjectBuilder = new JSObjectBuilder
  override def ArrayBuilder: JSArrayBuilder = new JSArrayBuilder
  override def String(value: java.lang.String): JSString = new JSString(value)
  override def Integer(value: java.lang.String): JSInteger = new JSInteger(value)
  override def Decimal(value: java.lang.String): JSDecimal = new JSDecimal(value)
  override def True: JSTrue.type = JSTrue
  override def False: JSFalse.type = JSFalse
  override def Null: JSNull.type = JSNull
}

object JSON extends JSON
