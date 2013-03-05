/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.containers._
import basis.control._

/** A latch that relays its result in the context of some trace.
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
class React[A](protected val trace: Trace) extends Latch[A] with Relay[A] {
  import MetaReact._
  
  @volatile private[process] final var state: AnyRef = Batch.empty[Latch[A]]
  
  final override def isSet: Boolean = state.isInstanceOf[_ Else _]
  
  final override def set(result: Try[A]): Boolean = {
    if (result == null) throw new NullPointerException
    var s = null: AnyRef
    do s = state
    while (!s.isInstanceOf[_ Else _] && !Unsafe.compareAndSwapObject(this, StateOffset, s, result))
    !s.isInstanceOf[_ Else _] && { s.asInstanceOf[Batch[Latch[A]]] traverse new Thunk.Signal(result); true }
  }
  
  override def forward[B >: A](that: Latch[B]): Unit = defer(that)
  
  override def run[U](f: Try[A] => U): Unit = defer(new Thunk.Run(f)(trace))
  
  override def foreach[U](f: A => U): Unit = defer(new Thunk.Foreach(f)(trace))
  
  override def andThen[U](q: PartialFunction[Try[A], U]): Relay[A] = {
    val r = new React[A](trace)
    defer(new Thunk.AndThen(q, r)(trace))
    r
  }
  
  override def choose[B](q: PartialFunction[A, B]): Relay[B] = {
    val r = new React[B](trace)
    defer(new Thunk.Choose(q, r)(trace))
    r
  }
  
  override def map[B](f: A => B): Relay[B] = {
    val r = new React[B](trace)
    defer(new Thunk.Map(f, r)(trace))
    r
  }
  
  override def flatMap[B](f: A => Relay[B]): Relay[B] = {
    val r = new React[B](trace)
    defer(new Thunk.FlatMap(f, r)(trace))
    r
  }
  
  override def recover[B >: A](q: PartialFunction[Throwable, B]): Relay[B] = {
    val r = new React[B](trace)
    defer(new Thunk.Recover(q, r)(trace))
    r
  }
  
  override def recoverWith[B >: A](q: PartialFunction[Throwable, Relay[B]]): Relay[B] = {
    val r = new React[B](trace)
    defer(new Thunk.RecoverWith(q, r)(trace))
    r
  }
  
  override def filter(p: A => Boolean): Relay[A] = {
    val r = new React[A](trace)
    defer(new Thunk.Filter(p, r)(trace))
    r
  }
  
  override def withFilter(p: A => Boolean): Relay[A] = {
    val r = new React[A](trace)
    defer(new Thunk.Filter(p, r)(trace))
    r
  }
  
  override def zip[B](that: Relay[B]): Relay[(A, B)] = {
    val r = new React.Zip[A, B](trace)
    this forward new r.Set1
    that forward new r.Set2
    r
  }
  
  private[process] final def defer(latch: Latch[A]) {
    if (latch == null) throw new NullPointerException
    var s = null: AnyRef
    do s = state
    while (!s.isInstanceOf[_ Else _] &&
           !Unsafe.compareAndSwapObject(this, StateOffset, state, state.asInstanceOf[Batch[Latch[A]]] :+ latch))
    if (s.isInstanceOf[_ Else _]) latch set s.asInstanceOf[Try[A]]
  }
}

private[basis] object React {
  private[basis] final class Zip[A, B](override val trace: Trace) extends React[(A, B)](trace) {
    @volatile private[process] final var _1: Try[A] = _
    
    @volatile private[process] final var _2: Try[B] = _
    
    def set1(result1: Try[A]) {
      if (result1 == null) throw new NullPointerException
      if (_1 == null) {
        _1 = result1
        if (result1.canTrap) set(result1.asInstanceOf[Nothing Else Throwable])
        else {
          val result2 = _2
          if (result2 != null && result2.canBind)
            set(Bind((result1.bind, result2.bind))) // safely races with set2
        }
      }
    }
    
    def set2(result2: Try[B]) {
      if (result2 == null) throw new NullPointerException
      if (_2 == null) {
        _2 = result2
        if (result2.canTrap) set(result2.asInstanceOf[Nothing Else Throwable])
        else {
          val result1 = _1
          if (result1 != null && result1.canBind)
            set(Bind((result1.bind, result2.bind))) // safely races with set1
        }
      }
    }
    
    final class Set1 extends Thunk[A](trace) {
      override def apply(result: Try[A]): Unit = set1(result)
    }
    
    final class Set2 extends Thunk[B](trace) {
      override def apply(result: Try[B]): Unit = set2(result)
    }
  }
}
