/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

sealed abstract class JSUndefined private[json] extends JSValue {
  override protected type Root = JSUndefined
  
  override def write(s: Appendable): Unit = s.append("undefined")
  
  override def toString: String = "undefined"
}

object JSUndefined extends JSUndefined
