/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

trait MapFactory[CC[A, T] <: Map[A, T]] {
  def apply[A, T](xs: (A, T)*)(implicit buffer: Buffer[CC[_, _], (A, T)]): buffer.State =
    macro MapFactory.apply[A, T]
}

private object MapFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A, T](c: Context)(xs: c.Expr[(A, T)]*)(buffer: c.Expr[Buffer[_, (A, T)]]): c.Expr[buffer.value.State] = {
    import c.universe._
    var b = Apply(Select(buffer.tree, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    c.Expr(Select(b, "state"))(TypeTag.Nothing)
  }
}
