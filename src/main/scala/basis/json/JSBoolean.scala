/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

sealed abstract class JSBoolean private[json] (val value: Boolean) extends JSValue {
  override protected type Root = JSBoolean
}

object JSBoolean {
  def apply(bool: Boolean): JSBoolean = if (bool) JSTrue else JSFalse
  
  def unapply(json: JSBoolean): Some[Boolean] = Some(json.value)
  
  object unary_+ extends runtime.AbstractPartialFunction[Any, JSBoolean] {
    override def isDefinedAt(x: Any): Boolean = x.isInstanceOf[JSBoolean]
    override def apply(x: Any): JSBoolean = x.asInstanceOf[JSBoolean]
    override def toString: String = "+JSBoolean"
  }
}

object JSTrue extends JSBoolean(true) {
  override def write(s: Appendable): Unit = s.append("true")
  
  override def toString: String = "true"
}

object JSFalse extends JSBoolean(false) {
  override def write(s: Appendable): Unit = s.append("false")
  
  override def toString: String = "false"
}
