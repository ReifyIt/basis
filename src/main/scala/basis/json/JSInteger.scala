/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

final class JSInteger(value: Long) extends JSNumber {
  override protected type Root = JSInteger
  
  def this(string: String) = this(java.lang.Long.valueOf(string))
  
  override def write(s: Appendable): Unit = s.append(toString)
  
  override def toInt: Int = value.toInt
  
  override def toLong: Long = value
  
  override def toFloat: Float = value.toFloat
  
  override def toDouble: Double = value.toDouble
  
  override def equals(other: Any): Boolean = other match {
    case that: JSInteger => toLong == that.toLong
    case _ => false
  }
  
  override def hashCode: Int = value.##
  
  override def toString: String = value.toString
}

object JSInteger {
  def apply(s: String): JSInteger = new JSInteger(s)
  
  def apply(n: Int): JSInteger = new JSInteger(n)
  
  def apply(n: Long): JSInteger = new JSInteger(n)
  
  def unapply(json: JSInteger): Some[Long] = Some(json.toLong)
  
  object unary_+ extends PartialFunction[Any, JSInteger] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSInteger]
    override def apply(x: Any): JSInteger = x.asInstanceOf[JSInteger]
    override def toString: String = "+JSInteger"
  }
}
