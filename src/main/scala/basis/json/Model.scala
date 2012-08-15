/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import basis.collection._
import basis.encoding._

trait Model {
  type Value
  
  type Object  <: Value
  
  type Array   <: Value
  
  type String  <: Value
  
  type Number  <: Value
  
  type Integer <: Number
  
  type Decimal <: Number
  
  type Boolean <: Value
  
  type Null    <: Value
  
  def ObjectBuilder: Builder[_, Value] { type Result <: Object }
  
  def ArrayBuilder: Builder[_, Value] { type Result <: Array }
  
  def String(value: utf8.String): String
  
  def Integer(value: utf8.String): Integer
  
  def Decimal(value: utf8.String): Decimal
  
  def True: Boolean
  
  def False: Boolean
  
  def Null: Null
}

object Model extends Model {
  import basis.json
  
  override type Value   = json.Value
  override type Object  = json.Object
  override type Array   = json.Array
  override type String  = json.String
  override type Number  = json.Number
  override type Integer = json.Integer
  override type Decimal = json.Decimal
  override type Boolean = json.Boolean
  override type Null    = json.Null.type
  
  override def ObjectBuilder: Builder[_, Value] { type Result <: json.Object } = ???
  
  override def ArrayBuilder: Builder[_, Value] { type Result <: json.Array } = ???
  
  override def String(value: utf8.String): json.String = ???
  
  override def Integer(value: utf8.String): json.Integer = ???
  
  override def Decimal(value: utf8.String): json.Decimal = ???
  
  override def True: json.True.type = json.True
  
  override def False: json.False.type = json.False
  
  override def Null: json.Null.type = json.Null
}
