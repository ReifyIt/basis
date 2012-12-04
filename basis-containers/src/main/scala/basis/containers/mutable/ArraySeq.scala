/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package mutable

import basis.collections._
import basis.collections.traversable._
import basis.memory._
import basis.util._

/** A contiguous mutable array.
  * 
  * @groupprio  Examining     -6
  * @groupprio  Modifying     -5
  * @groupprio  Copying       -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  array sequence
  */
abstract class ArraySeq[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) A]
  extends Equals
    with Mutable
    with Family[ArraySeq[A]]
    with mutable.IndexedSeq[A] {
  
  override def apply(index: Int): A
  
  override def update(index: Int, value: A): Unit
  
  /** Copies elementes from this $collection to an array slice.
    * 
    * @param  xs      the destination array.
    * @param  start   the offset to copy to in the destination array.
    * @param  count   the maximum number of elements to copy.
    * @group  Copying
    */
  def copyToArray(xs: Array[A], start: Int, count: Int) {
    var i = 0
    var j = start
    val n = count min (xs.length - start) min length
    while (i < n) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }
  
  /** Copies elementes from this $collection to an array offset.
    * 
    * @param  xs      the destination array.
    * @param  start   the offset to copy to in the destination array.
    * @group  Copying
    */
  def copyToArray(xs: Array[A], start: Int) {
    var i = 0
    var j = start
    val n = (xs.length - start) min length
    while (i < n) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }
  
  /** Copies elementes from this $collection to an array.
    * 
    * @param  xs  the destination array.
    * @group  Copying
    */
  def copyToArray(xs: Array[A]) {
    var i = 0
    val n = xs.length min length
    while (i < n) {
      xs(i) = this(i)
      i += 1
    }
  }
  
  protected override def stringPrefix: String = "ArraySeq"
}

object ArraySeq {
  def apply[A](xs: A*)(implicit struct: DataType[A]): ArraySeq[A] =
    macro ArraySeqMacros.apply[A]
  
  implicit def Builder[A](implicit struct: DataType[A]): Builder[Any, A] { type State = ArraySeq[A] } = {
    import ValType._
    (struct match {
      case _ : RefType[A]              => new RefArraySeqBuilder[A]
      case PackedByte                  => new ByteArraySeqBuilder
      case PackedShort  | PaddedShort  => new ShortArraySeqBuilder
      case PackedInt    | PaddedInt    => new IntArraySeqBuilder
      case PackedLong   | PaddedLong   => new LongArraySeqBuilder
      case PackedFloat  | PaddedFloat  => new FloatArraySeqBuilder
      case PackedDouble | PaddedDouble => new DoubleArraySeqBuilder
      case PackedBoolean               => new BitArraySeqBuilder
      case struct: ValType[A]          => new ValArraySeqBuilder[A]()(struct)
    }).asInstanceOf[Builder[Any, A] { type State = ArraySeq[A] }]
  }
}

private[containers] object ArraySeqMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A : c.WeakTypeTag](c: Context)(xs: c.Expr[A]*)(struct: c.Expr[DataType[A]]): c.Expr[ArraySeq[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val ArraySeqType =
      appliedType(
        mirror.staticClass("basis.containers.mutable.ArraySeq").toType,
        weakTypeOf[A] :: Nil)
    var b =
      Apply(
        TypeApply(
          Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
            "basis"), "containers"), "mutable"), "ArraySeq"), "Builder"),
          TypeTree(weakTypeOf[A]) :: Nil),
        struct.tree :: Nil)
    b = Apply(Select(b, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(b, "state"))(WeakTypeTag(ArraySeqType))
  }
}
