/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

abstract class JSNumber extends JSValue {
  override protected type Root >: this.type <: JSNumber
  
  def toInt: Int
  
  def toLong: Long
  
  def toFloat: Float
  
  def toDouble: Double
}

object JSNumber {
  def parse(string: String): JSNumber = {
    val parser = new JSONReader[JSON.type](string)
    parser.skipWhitespace()
    parser.parseJSNumber[JSON.type](JSON)
  }
  
  object unary_+ extends PartialFunction[Any, JSNumber] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSNumber]
    override def apply(x: Any): JSNumber = x.asInstanceOf[JSNumber]
    override def toString: String = "+JSNumber"
  }
}
