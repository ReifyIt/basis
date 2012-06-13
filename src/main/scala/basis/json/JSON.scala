/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
import basis.json

import scala.collection.mutable.Builder

object JSON extends JSONContext {
  override type JSValue = json.JSValue
  override type JSObject = json.JSObject
  override type JSArray = json.JSArray
  override type JSString = json.JSString
  override type JSNumber = json.JSNumber
  override type JSInteger = json.JSInteger
  override type JSDecimal = json.JSDecimal
  override type JSBoolean = json.JSBoolean
  override type JSNull = json.JSNull
  
  override def JSObjectBuilder: json.JSObjectBuilder = new json.JSObjectBuilder
  override def JSArrayBuilder: json.JSArrayBuilder = new json.JSArrayBuilder
  override def JSString(string: String): json.JSString = new json.JSString(string)
  override def JSInteger(string: String): json.JSInteger = new json.JSInteger(string)
  override def JSDecimal(string: String): json.JSDecimal = new json.JSDecimal(string)
  override def JSTrue: json.JSTrue.type = json.JSTrue
  override def JSFalse: json.JSFalse.type = json.JSFalse
  override def JSNull: json.JSNull.type = json.JSNull
}
