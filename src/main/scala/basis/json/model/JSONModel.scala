/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

trait JSONModel {
  type Value
  
  type Object  <: Value
  
  type Array   <: Value
  
  type String  <: Value
  
  type Number  <: Value
  
  type Integer <: Number
  
  type Decimal <: Number
  
  type Boolean <: Value
  
  type Null    <: Value
  
  def ObjectBuilder: JSONObjectBuilder[Object]
  
  def ArrayBuilder: JSONArrayBuilder[Array]
  
  def String(value: java.lang.String): String
  
  def Integer(value: java.lang.String): Integer
  
  def Decimal(value: java.lang.String): Decimal
  
  def True: Boolean
  
  def False: Boolean
  
  def Null: Null
}
