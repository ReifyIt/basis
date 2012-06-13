/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

final class JSDecimal(value: Double) extends JSNumber {
  override protected type Root = JSDecimal
  
  def this(string: String) = this(java.lang.Double.valueOf(string))
  
  override def write(s: Appendable): Unit = s.append(toString)
  
  override def toInt: Int = value.toInt
  
  override def toLong: Long = value.toLong
  
  override def toFloat: Float = value.toFloat
  
  override def toDouble: Double = value
  
  override def equals(other: Any): Boolean = other match {
    case that: JSDecimal => toDouble == that.toDouble
    case _ => false
  }
  
  override def hashCode: Int = value.##
  
  override def toString: String =
    if (!java.lang.Double.isNaN(value) &&
        !java.lang.Double.isInfinite(value)) value.toString else "null"
}

object JSDecimal {
  def apply(s: String): JSDecimal = new JSDecimal(s)
  
  def apply(x: Float): JSDecimal = new JSDecimal(x)
  
  def apply(x: Double): JSDecimal = new JSDecimal(x)
  
  def unapply(json: JSDecimal): Some[Double] = Some(json.toDouble)
  
  object unary_+ extends PartialFunction[Any, JSDecimal] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSDecimal]
    override def apply(x: Any): JSDecimal = x.asInstanceOf[JSDecimal]
    override def toString: String = "+JSDecimal"
  }
}
