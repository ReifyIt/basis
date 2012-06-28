/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

trait JSONProxyModel[J <: JSONModel] extends JSONModel {
  def self: J
  
  type Value   = J#Value
  type Object  = J#Object
  type Array   = J#Array
  type String  = J#String
  type Number  = J#Number
  type Integer = J#Integer
  type Decimal = J#Decimal
  type Boolean = J#Boolean
  type Null    = J#Null
  
  def ObjectBuilder: JSONObjectBuilder[J#Object] = self.ObjectBuilder
  def ArrayBuilder: JSONArrayBuilder[J#Array] = self.ArrayBuilder
  def String(string: java.lang.String): J#String = self.String(string)
  def Integer(string: java.lang.String): J#Integer = self.Integer(string)
  def Decimal(string: java.lang.String): J#Decimal = self.Decimal(string)
  def True: J#Boolean = self.True
  def False: J#Boolean = self.False
  def Null: J#Null = self.Null
}
