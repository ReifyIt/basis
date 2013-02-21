/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

import basis.containers._
import basis.control._

import scala.runtime.AbstractFunction0
import scala.runtime.AbstractFunction1

/** A pending computation relayed by the default async implementation.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  * @groupprio  Relaying    2
  * @groupprio  Composing   3
  * @groupprio  Recovering  4
  */
private[basis] class Thunk[A] extends Latch[A] with Relay[A] {
  import MetaThunk._
  
  @volatile private[dispatch] final var state: AnyRef = Batch.empty[Latch[A]]
  
  final override def isSet: Boolean = state.isInstanceOf[_ Else _]
  
  final override def set(result: Try[A]): Boolean = {
    if (result == null) throw new NullPointerException
    var s = null: AnyRef
    do s = state
    while (!s.isInstanceOf[_ Else _] && !Unsafe.compareAndSwapObject(this, StateOffset, s, result))
    !s.isInstanceOf[_ Else _] && { s.asInstanceOf[Batch[Latch[A]]] traverse new Thunk.Trigger(result); true }
  }
  
  final override def forward[B >: A](that: Latch[B]): Unit = defer(that)
  
  override def run[U](f: Try[A] => U): Unit = defer(new Thunk.Run(f))
  
  override def foreach[U](f: A => U): Unit = defer(new Thunk.Foreach(f))
  
  override def andThen[U](q: PartialFunction[Try[A], U]): Relay[A] = {
    val t = new Thunk[A]
    defer(new Thunk.AndThen(q, t))
    t
  }
  
  override def choose[B](q: PartialFunction[A, B]): Relay[B] = {
    val t = new Thunk[B]
    defer(new Thunk.Choose(q, t))
    t
  }
  
  override def map[B](f: A => B): Relay[B] = {
    val t = new Thunk[B]
    defer(new Thunk.Map(f, t))
    t
  }
  
  override def flatMap[B](f: A => Relay[B]): Relay[B] = {
    val t = new Thunk[B]
    defer(new Thunk.FlatMap(f, t))
    t
  }
  
  override def recover[B >: A](q: PartialFunction[Throwable, B]): Relay[B] = {
    val t = new Thunk[B]
    defer(new Thunk.Recover(q, t))
    t
  }
  
  override def recoverWith[B >: A](q: PartialFunction[Throwable, Relay[B]]): Relay[B] = {
    val t = new Thunk[B]
    defer(new Thunk.RecoverWith(q, t))
    t
  }
  
  override def filter(p: A => Boolean): Relay[A] = {
    val t = new Thunk[A]
    defer(new Thunk.Filter(p, t))
    t
  }
  
  override def withFilter(p: A => Boolean): Relay[A] = {
    val t = new Thunk[A]
    defer(new Thunk.Filter(p, t))
    t
  }
  
  override def zip[B](that: Relay[B]): Relay[(A, B)] = {
    val t = new Thunk.Zip[A, B]
    this forward new t.Set1
    that forward new t.Set2
    t
  }
  
  private[basis] final def defer(latch: Latch[A]) {
    if (latch == null) throw new NullPointerException
    var s = null: AnyRef
    do s = state
    while (!s.isInstanceOf[_ Else _] &&
           !Unsafe.compareAndSwapObject(this, StateOffset, state, state.asInstanceOf[Batch[Latch[A]]] :+ latch))
    if (s.isInstanceOf[_ Else _]) latch set s.asInstanceOf[Try[A]]
  }
}

private[basis] object Thunk {
  class Group[A] extends Thunk[Batch[A]] {
    import MetaThunk._
    import MetaGroup._
    
    @volatile private[dispatch] final var store: Batch[A] = Batch.empty[A]
    
    /** The number of results to accumulate; unbounded when negative */
    @volatile final var size: Int = 0
    
    /** Adds a successful result, setting this thunk upon accumulating `size`
      * elements, or failing this thunk if an element traps. */
    final def put(elem: Try[A]): Boolean = {
      if (elem == null) throw new NullPointerException
      if (elem.canTrap) set(elem.asInstanceOf[Nothing Else Throwable])
      else {
        var b = null: Batch[A]
        var n = 0
        var s = 0
        do { b = store; s = size; n = b.length }
        while ((n < s || s < 0) && !Unsafe.compareAndSwapObject(this, StoreOffset, b, b :+ elem.bind))
        if (n == size - 1) set(Bind(store)) // recheck size
        true
      }
    }
    
    /** Negates `size` and sets this thunk if complete. */
    final def commit() {
      val s = -size
      size = s
      val b = store // must read after updating size
      if (b.length == s) set(Bind(b)) // safely races with last put
    }
    
    /** Puts the result of a thunk in this group. */
    final class Put(thunk: () => A) extends AbstractFunction0[Unit] {
      override def apply(): Unit = put(try Bind(thunk()) catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  abstract class When[-A] extends AbstractFunction0[Unit] with Latch[A] {
    @volatile protected[this] final var value: Try[A] = _
    final override def isSet: Boolean = value != null
    final override def set(result: Try[A]): Boolean = {
      if (result == null) throw new NullPointerException
      if (value == null) {
        value = result
        async.exec(this)
        true
      }
      else false
    }
    final override def apply() {
      val r = value
      if (r == null) throw new NullPointerException
      apply(r)
    }
    def apply(result: Try[A]): Unit
  }
  
  final class Eval[-A](expr: => A, t: Latch[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      t set (try Bind(expr) catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  final class Exec[-A](thunk: () => A, t: Latch[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      t set (try Bind(thunk()) catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  final class Race[-A](thunk: () => A, t: Latch[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!t.isSet) t set (try Bind(thunk()) catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  final class Run[-A](f: Try[A] => _) extends When[A] {
    override def apply(r: Try[A]): Unit = f(r)
  }
  
  final class Foreach[-A](f: A => _) extends When[A] {
    override def apply(r: Try[A]): Unit = if (r.canBind) f(r.bind)
  }
  
  final class AndThen[-A](q: PartialFunction[Try[A], _], t: Latch[A]) extends When[A] {
    override def apply(r: Try[A]) {
      try if (q.isDefinedAt(r)) q(r)
      finally t set r
    }
  }
  
  final class Choose[-A, +B](q: PartialFunction[A, B], t: Latch[B]) extends When[A] {
    override def apply(r: Try[A]) {
      t set (try if (r.canBind) { if (q.isDefinedAt(r.bind)) Bind(q(r.bind)) else Trap }
                 else r.asInstanceOf[Nothing Else Throwable]
             catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  final class Map[-A, +B](f: A => B, t: Latch[B]) extends When[A] {
    override def apply(r: Try[A]) {
      t set (try if (r.canBind) Bind(f(r.bind)) else r.asInstanceOf[Nothing Else Throwable]
             catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  final class FlatMap[-A, +B](f: A => Relay[B], t: Latch[B]) extends When[A] {
    override def apply(r: Try[A]) {
      try if (r.canBind) f(r.bind) forward t else t set r.asInstanceOf[Nothing Else Throwable]
      catch { case e: Throwable => t set Trap.NonFatal(e) }
    }
  }
  
  final class Recover[-A](q: PartialFunction[Throwable, A], t: Latch[A]) extends When[A] {
    override def apply(r: Try[A]) {
      t set (try if (r.canSafelyTrap && q.isDefinedAt(r.trap)) Bind(q(r.trap)) else r
             catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  final class RecoverWith[-A](q: PartialFunction[Throwable, Relay[A]], t: Latch[A]) extends When[A] {
    override def apply(r: Try[A]) {
      try if (r.canSafelyTrap && q.isDefinedAt(r.trap)) q(r.trap) forward t else t set r
      catch { case e: Throwable => t set Trap.NonFatal(e) }
    }
  }
  
  final class Filter[-A](p: A => Boolean, t: Latch[A]) extends When[A] {
    override def apply(r: Try[A]) {
      t set (try if (r.canTrap || p(r.bind)) r else Trap
             catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  final class Zip[A, B] extends Thunk[(A, B)] {
    @volatile private[dispatch] final var _1: Try[A] = _
    @volatile private[dispatch] final var _2: Try[B] = _
    def set1(r1: Try[A]) {
      if (r1 == null) throw new NullPointerException
      if (_1 == null) {
        _1 = r1
        if (r1.canTrap) set(r1.asInstanceOf[Nothing Else Throwable])
        else {
          val r2 = _2
          if (r2 != null && r2.canBind) set(Bind((r1.bind, r2.bind)))
        }
      }
    }
    def set2(r2: Try[B]) {
      if (r2 == null) throw new NullPointerException
      if (_2 == null) {
        _2 = r2
        if (r2.canTrap) set(r2.asInstanceOf[Nothing Else Throwable])
        else {
          val r1 = _1
          if (r1 != null && r1.canBind) set(Bind((r1.bind, r2.bind)))
        }
      }
    }
    final class Set1 extends When[A] {
      override def apply(r: Try[A]): Unit = set1(r)
    }
    final class Set2 extends When[B] {
      override def apply(r: Try[B]): Unit = set2(r)
    }
  }
  
  final class Trigger[+A](r: Try[A]) extends AbstractFunction1[Latch[A], Unit] {
    override def apply(latch: Latch[A]): Unit = latch set r
  }
}
