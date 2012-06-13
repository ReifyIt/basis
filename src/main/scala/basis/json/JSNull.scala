/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

sealed abstract class JSNull private[json] extends JSValue {
  override protected type Root = JSNull
  
  override def write(s: Appendable): Unit = s.append("null")
  
  override def toString: String = "null"
}

object JSNull extends JSNull
