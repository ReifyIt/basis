/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

trait JSONObjectModel extends JSONModel {
  def ObjectBuilderForName(name: java.lang.String): JSONObjectBuilder[Object] = ObjectBuilder
  
  def ArrayBuilderForName(name: java.lang.String): JSONArrayBuilder[Array] = ArrayBuilder
  
  def StringForName(name: java.lang.String, value: java.lang.String): String = String(value)
  
  def IntegerForName(name: java.lang.String, value: java.lang.String): Integer = Integer(value)
  
  def DecimalForName(name: java.lang.String, value: java.lang.String): Decimal = Decimal(value)
  
  def TrueForName(name: java.lang.String): Boolean = True
  
  def FalseForName(name: java.lang.String): Boolean = False
  
  def NullForName(name: java.lang.String): Null = Null
}
