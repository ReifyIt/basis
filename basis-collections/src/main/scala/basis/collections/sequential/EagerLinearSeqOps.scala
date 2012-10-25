/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Strictly evaluated linear sequence operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
abstract class EagerLinearSeqOps[+Self, +A] private[sequential] {
  /** Returns the applications of a partial function to each element in this
    * sequence for which the function is defined.
    * 
    * @param  q       the partial function to filter elements against and to
    *                 apply to applicable elements.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro EagerLinearSeqOps.collect[A, B]
  
  /** Returns the applications of a function to each element in this sequence.
    * 
    * @param  f       the function to apply to each element.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State =
    macro EagerLinearSeqOps.map[A, B]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this sequence.
    * 
    * @param  f       the enumerator-yielding function to apply to each element.
    * @param  buffer  the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro EagerLinearSeqOps.flatMap[A, B]
  
  /** Returns all elements in this sequence that satisfy a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro EagerLinearSeqOps.filter[A]
  
  /** Returns all elements following the longest prefix of this sequence
    * for which each element satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro EagerLinearSeqOps.dropWhile[A]
  
  /** Returns the longest prefix of this sequence for which each element
    * satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro EagerLinearSeqOps.takeWhile[A]
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  bufferA   the implicit accumulator for prefix elements.
    * @param  bufferB   the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  //FIXME: SI-6447
  //def span(p: A => Boolean)(
  //    implicit builderA: Buffer[Self, A],
  //             builderB: Buffer[Self, A])
  //  : (builderA.State, builderB.State) =
  //  macro EagerLinearSeqOps.span[A]
  
  /** Returns all elements in this sequence following a prefix up to some length.
    * 
    * @param  lower   the length of the prefix to drop;
    *                 also the inclusive lower bound for indexes of elements to keep.
    * @param  buffer  the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro EagerLinearSeqOps.drop[A]
  
  /** Returns a prefix of this sequence up to some length.
    * 
    * @param  upper   the length of the prefix to take;
    *                 also the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro EagerLinearSeqOps.take[A]
  
  /** Returns an interval of elements in this sequence.
    * 
    * @param  lower   the inclusive lower bound for indexes of elements to keep.
    * @param  upper   the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro EagerLinearSeqOps.slice[A]
  
  /** Returns pairs of elements from this and another sequence.
    * 
    * @param  that    the container whose elements to pair with these elements.
    * @param  buffer  the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](that: LinearSeq[B])(implicit buffer: Buffer[Self, (A, B)]): buffer.State =
    macro EagerLinearSeqOps.zip[A, B]
  
  /** Returns the concatenation of this and another sequence.
    * 
    * @param  that    the container to append to this sequence.
    * @param  buffer  the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both containers.
    * @group  Combining
    */
  def ++ [B >: A](that: LinearSeq[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro EagerLinearSeqOps.++[A, B]
}

private[sequential] object EagerLinearSeqOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct(c: Context): c.Tree = {
    import c.universe._
    val Apply(_, seq :: Nil) = c.prefix.tree
    seq
  }
  
  def collect[A : c.WeakTypeTag, B]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (buffer: c.Expr[Buffer[_, B]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.collect[A, B](c)(deconstruct(c), q.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def map[A : c.WeakTypeTag, B]
      (c: Context)
      (f: c.Expr[A => B])
      (buffer: c.Expr[Buffer[_, B]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.map[A, B](c)(deconstruct(c), f.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def flatMap[A : c.WeakTypeTag, B]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (buffer: c.Expr[Buffer[_, B]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.flatMap[A, B](c)(deconstruct(c), f.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.filter[A](c)(deconstruct(c), p.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.dropWhile[A](c)(deconstruct(c), p.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.takeWhile[A](c)(deconstruct(c), p.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (bufferA: c.Expr[Buffer[_, A]], bufferB: c.Expr[Buffer[_, A]])
    : c.Expr[(bufferA.value.State, bufferB.value.State)] =
    c.Expr {
      LinearSeqMacros.span[A](c)(deconstruct(c), p.tree, bufferA.tree, bufferB.tree)
    } (Tuple2Tag(c)(BufferStateTag(c)(bufferA), BufferStateTag(c)(bufferB)))
  
  def drop[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.drop[A](c)(deconstruct(c), lower.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def take[A : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.take[A](c)(deconstruct(c), upper.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def slice[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.slice[A](c)(deconstruct(c), lower.tree, upper.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[LinearSeq[B]])
      (buffer: c.Expr[Buffer[_, (A, B)]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    c.Expr {
      LinearSeqMacros.zip[A, B](c)(deconstruct(c), that.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  }
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[LinearSeq[B]])
      (buffer: c.Expr[Buffer[_, B]])
    : c.Expr[buffer.value.State] =
    c.Expr {
      LinearSeqMacros.++[A, B](c)(deconstruct(c), that.tree, buffer.tree)
    } (BufferStateTag(c)(buffer))
  
  private def BufferStateTag
      (c: Context)
      (buffer: c.Expr[Buffer[_, _]])
    : c.WeakTypeTag[buffer.value.State] = {
    import c.universe._
    c.WeakTypeTag(
      typeRef(
        singleType(NoPrefix, buffer.staticType.typeSymbol),
        buffer.staticType.member(newTypeName("state")), Nil))
  }
  
  private def Tuple2Tag[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
    : c.WeakTypeTag[(A, B)] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("scala.Tuple2").toType,
        weakTypeOf[A] :: weakTypeOf[B] :: Nil))
  }
}
