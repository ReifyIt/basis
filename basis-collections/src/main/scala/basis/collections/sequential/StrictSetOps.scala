/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Strictly evaluated set operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
abstract class StrictSetOps[A, Family] private[sequential] {
  /** Returns the applications of a partial function to each element in this
    * set for which the function is defined.
    * 
    * @param  q         the partial function to filter elements against and
    *                   to apply to applicable elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[Family, B, To]): To =
    macro StrictContainerOps.collect[A, B, To]
  
  /** Returns the applications of a function to each element in this set.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[Family, B, To]): To =
    macro StrictContainerOps.map[A, B, To]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this set.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[Family, B, To]): To =
    macro StrictContainerOps.flatMap[A, B, To]
  
  /** Returns all elements in this set that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    macro StrictContainerOps.filter[A, To]
  
  /** Returns all elements following the longest prefix of this set
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    macro StrictContainerOps.dropWhile[A, To]
  
  /** Returns the longest prefix of this set for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    macro StrictContainerOps.takeWhile[A, To]
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p           the predicate to test elements against.
    * @param  builderA    the implicit accumulator for prefix elements.
    * @param  builderB    the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span[To](p: A => Boolean)(
      implicit builderA: Builder[Family, A, To],
               builderB: Builder[Family, A, To])
    : (To, To) =
    macro StrictContainerOps.span[A, To]
  
  /** Returns all elements in this set following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[Family, A, To]): To =
    macro StrictContainerOps.drop[A, To]
  
  /** Returns a prefix of this set up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[Family, A, To]): To =
    macro StrictContainerOps.take[A, To]
  
  /** Returns an interval of elements in this set.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[Family, A, To]): To =
    macro StrictContainerOps.slice[A, To]
  
  /** Returns pairs of elements from this and another set.
    * 
    * @param  that      the container whose elements to pair with these elements.
    * @param  builder   the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B, To](that: Container[B])(implicit builder: Builder[Family, (A, B), To]): To =
    macro StrictContainerOps.zip[A, B, To]
  
  /** Returns the concatenation of this and another set.
    * 
    * @param  that      the container to append to this set.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both containers.
    * @group  Combining
    */
  def ++ [B >: A, To](that: Container[B])(implicit builder: Builder[Family, B, To]): To =
    macro StrictContainerOps.++[A, B, To]
}
