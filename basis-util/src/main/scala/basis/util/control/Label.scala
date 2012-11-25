/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util
package control

class Break extends Throwable {
  override def fillInStackTrace(): Throwable = this
}

class Label {
  val signal: Break = new Break
  
  def apply(op: Unit): Unit =
    macro LabelMacros.apply
  
  def break(): Nothing =
    macro LabelMacros.break
}

private[control] object LabelMacros {
  import scala.reflect.macros.Context
  
  def apply(c: Context { type PrefixType <: Label })(op: c.Expr[Unit]): c.Expr[Unit] = c.universe.reify {
    try op.splice
    catch { case signal: Break if signal eq c.prefix.splice.signal => () }
  }
  
  def break(c: Context { type PrefixType <: Label })(): c.Expr[Nothing] = c.universe.reify {
    throw c.prefix.splice.signal
  }
}
