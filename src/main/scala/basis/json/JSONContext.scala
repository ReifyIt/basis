/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.collection.mutable.Builder

trait JSONContext {
  type JSValue
  
  type JSObject  <: JSValue
  
  type JSArray   <: JSValue
  
  type JSString  <: JSValue
  
  type JSNumber  <: JSValue
  
  type JSInteger <: JSNumber
  
  type JSDecimal <: JSNumber
  
  type JSBoolean <: JSValue
  
  type JSNull    <: JSValue
  
  def JSObjectBuilder: Builder[(String, JSValue), JSObject]
  
  def JSArrayBuilder: Builder[JSValue, JSArray]
  
  def JSString(string: String): JSString
  
  def JSInteger(string: String): JSInteger
  
  def JSDecimal(string: String): JSDecimal
  
  def JSTrue: JSBoolean
  
  def JSFalse: JSBoolean
  
  def JSNull: JSNull
}
