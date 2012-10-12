/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

import basis._

/** A bounded enumeration of monotonic `Int` values. The sequence begins at
  * the `start` value and repeatedly adds the non-zero `step` value until it
  * either surpasses the `end` value or overflows. This definition
  * deliberately implies the following `foreach` implementation:
  *
  * {{{
  * def foreach[U](f: Int => U) {
  *   var i = start
  *   if (step > 0) {
  *     if (isInclusive) while (i <= end) { f(i); i += step }
  *     else             while (i <  end) { f(i); i += step }
  *   }
  *   else { // step < 0
  *     if (isInclusive) while (i >= end) { f(i); i += step }
  *     else             while (i >  end) { f(i); i += step }
  *   }
  * }
  * }}}
  * 
  * A macro expands the appropriate foreach incantation for literal ranges
  * that have literal step and inclusivity. As a result, for comprehensions
  * over such ranges reduce to their equivalent while loops.
  * 
  * An implicit conversion from `Int` to [[IntOps]] turns the notation `a to b`
  * and `a until b` into an inclusive or exclusive literal range, respectively.
  * 
  * @author Chris Sachs
  */
class Range(val start: Int, val end: Int, val step: Int, val isInclusive: Boolean) extends Enumerator[Int] {
  if (step == 0) throw new java.lang.IllegalArgumentException("Range step must be non-zero.")
  
  import scala.language.experimental.macros
  
  def by(step: Int): Range = macro RangeMacros.by
  
  override protected def foreach[U](f: Int => U) {
    var i = start
    val s = step
    val n = end
    if (s > 0) {
      if (isInclusive) while (i <= n) { f(i); i += s }
      else             while (i <  n) { f(i); i += s }
    }
    else if (s < 0) {
      if (isInclusive) while (i >= n) { f(i); i += s }
      else             while (i <  n) { f(i); i += s }
    }
    else throw new scala.MatchError(s)
  }
}
