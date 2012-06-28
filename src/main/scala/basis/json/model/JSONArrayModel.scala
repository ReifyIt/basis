/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

trait JSONArrayModel extends JSONModel {
  def ObjectBuilderForIndex(index: Int): JSONObjectBuilder[Object] = ObjectBuilder
  
  def ArrayBuilderForIndex(index: Int): JSONArrayBuilder[Array] = ArrayBuilder
  
  def StringForIndex(index: Int, value: java.lang.String): String = String(value)
  
  def IntegerForIndex(index: Int, value: java.lang.String): Integer = Integer(value)
  
  def DecimalForIndex(index: Int, value: java.lang.String): Decimal = Decimal(value)
  
  def TrueForIndex(index: Int): Boolean = True
  
  def FalseForIndex(index: Int): Boolean = False
  
  def NullForIndex(index: Int): Null = Null
}
