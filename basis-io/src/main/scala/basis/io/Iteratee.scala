/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.io

import basis.collections._
import basis.control._

/** An iterator consumer. Iteratees take an input iterator and return either
  * a result or an iteratee that consumes more input.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
trait Iteratee[I, +O] {
  /** Feeds an input iterator to this iteratee and returns either a result or
    * an iteratee that consumes more input. */
  def apply(input: Iterator[I]): O Else Iteratee[I, O]
  
  /** Returns an iteratee that applies a function to the result of this iteratee. */
  def map[P](f: O => P): Iteratee[I, P] = new Iteratee.Map(this, f)
  
  /** Applies the result of this iteratee to an iteratee-returning function,
    * and returns the remaining input applied to that iteratee. */
  def flatMap[P](f: O => Iteratee[I, P]): Iteratee[I, P] = new Iteratee.FlatMap(this, f)
}

/** A factory for iteratees. */
object Iteratee {
  /** Returns an iteratee that applies a function to each input element.
    * The iteratee binds `()` when its input `isDone`. */
  def foreach[I](f: I => Unit): Iteratee[I, Unit] = new Foreach(f)
  
  /** Returns an iteratee that folds each input element with an operator
    * function, starting with some zero value. The iteratee binds the folded
    * result when its input `isDone`. */
  def fold[I, O](z: O)(op: (O, I) => O): Iteratee[I, O] = new Fold(z, op)
  
  /** Returns an iteratee that binds the first element of its input. The
    * iteratee binds the `Trap` value when its input `isDone`. */
  def first[I]: Iteratee[I, Maybe[I]] = new First
  
  private final class Foreach[I](f: I => Unit) extends Iteratee[I, Unit] {
    override def apply(input: Iterator[I]): Unit Else Iteratee[I, Unit] = {
      while (!input.isEmpty) {
        f(input.head)
        input.step()
      }
      if (input.isDone) Bind(()) else Trap(this)
    }
  }
  
  private final class Fold[I, +O](private[this] var state: O, op: (O, I) => O) extends Iteratee[I, O] {
    override def apply(input: Iterator[I]): O Else Iteratee[I, O] = {
      while (!input.isEmpty) {
        state = op(state, input.head)
        input.step()
      }
      if (input.isDone) Bind(state) else Trap(this)
    }
  }
  
  private final class First[I] extends Iteratee[I, Maybe[I]] {
    override def apply(input: Iterator[I]): Maybe[I] Else Iteratee[I, Maybe[I]] = {
      if (!input.isEmpty) Bind(Bind(input.head))
      else if (input.isDone) Bind(Trap)
      else Trap(this)
    }
  }
  
  private final class Map[I, +O, +P](self: Iteratee[I, O], f: O => P) extends Iteratee[I, P] {
    override def apply(input: Iterator[I]): P Else Iteratee[I, P] = {
      val step = self(input)
      if (step.canBind) Bind(f(step.bind)) else Trap(this)
    }
  }
  
  private final class FlatMap[I, +O, +P](self: Iteratee[I, O], f: O => Iteratee[I, P]) extends Iteratee[I, P] {
    override def apply(input: Iterator[I]): P Else Iteratee[I, P] = {
      val step = self(input)
      if (step.canBind) f(step.bind)(input) else Trap(this)
    }
  }
}
