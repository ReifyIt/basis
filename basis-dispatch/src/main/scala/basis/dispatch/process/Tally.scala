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

import scala.runtime.AbstractFunction0

/** A latch that joins pieces together and relays the compound result.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Tallying    1
  * @groupprio  Evaluating  2
  * @groupprio  Relaying    3
  * @groupprio  Composing   4
  * @groupprio  Recovering  5
  */
abstract class Tally[-A, B](trace: Trace) extends React[B](trace) with Plate[A] {
  /** Returns the expected number of pieces.
    * @group Tallying */
  def limit: Int
  
  /** Sets the number of pieces to accept; unbounded when negative.
    * @group Tallying */
  def limit_=(count: Int): Unit
  
  /** Negates `limit`, and latches if complete.
    * @group Tallying */
  def commit(): Unit
}

private[basis] object Tally {
  import MetaTally._
  
  private[basis] final class JoinAll[A](trace: Trace) extends Tally[A, Batch[A]](trace) {
    @volatile private[process] var batch: Batch[A] = Batch.empty[A]
    
    @volatile override var limit: Int = 0
    
    override def put(result: Try[A]) {
      if (result == null) throw new NullPointerException
      if (result.canBind) putBind(result.bind)
      else set(result.asInstanceOf[Nothing Else Throwable])
    }
    
    override def putBind(value: A) {
      var b = null: Batch[A]
      var n = 0
      var k = 0
      do { b = batch; k = limit; n = b.length }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapObject(this, JoinAllBatchOffset, b, b :+ value))
      k = limit // recheck limit
      if (n == k - 1) set(Bind(batch))
    }
    
    override def putTrap(exception: Throwable): Unit = set(Trap(exception))
    
    override def commit() {
      val k = -limit
      limit = k
      val b = batch // must read after updating limit
      if (b.length == k) set(Bind(b)) // safely races with last put
    }
  }
  
  private[basis] final class JoinAny[A](trace: Trace) extends Tally[A, A](trace) {
    @volatile private[process] var count: Int = 0
    
    @volatile override var limit: Int = 0
    
    override def put(result: Try[A]) {
      if (result == null) throw new NullPointerException
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, JoinAnyCountOffset, n, n + 1))
      k = limit // recheck limit
      if (n == k - 1 || (n < k || k < 0) && result.canBind) set(result)
    }
    
    override def putBind(value: A) {
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, JoinAnyCountOffset, n, n + 1))
      k = limit // recheck limit
      if (n < k || k < 0) set(Bind(value))
    }
    
    override def putTrap(exception: Throwable) {
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, JoinAnyCountOffset, n, n + 1))
      k = limit // recheck limit
      if (n == k - 1) set(Trap(exception))
    }
    
    override def commit() {
      val k = -limit
      limit = k
      val n = count // must read after updating limit
      if (n == k) set(Trap) // safely races with put
    }
  }
  
  private[basis] final class JoinFirst[A](p: A => Boolean)(trace: Trace) extends Tally[A, A](trace) {
    @volatile private[process] var count: Int = 0
    
    @volatile override var limit: Int = 0
    
    override def put(result: Try[A]) {
      if (result == null) throw new NullPointerException
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, JoinFirstCountOffset, n, n + 1))
      k = limit // recheck limit
      if ((n < k || k < 0) && result.canBind && p(result.bind)) set(result)
      else if (n == k - 1) set(Trap)
    }
    
    override def putBind(value: A) {
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, JoinFirstCountOffset, n, n + 1))
      k = limit // recheck limit
      if ((n < k || k < 0) && p(value)) set(Bind(value))
      else if (n == k - 1) set(Trap)
    }
    
    override def putTrap(exception: Throwable) {
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, JoinFirstCountOffset, n, n + 1))
      k = limit // recheck limit
      if (n == k - 1) set(Trap(exception))
    }
    
    override def commit() {
      val k = -limit
      limit = k
      val n = count // must read after updating limit
      if (n == k) set(Trap) // safely races with put
    }
  }
  
  private[basis] final class Reduce[A](z: A)(op: (A, A) => A)(override val trace: Trace) extends Tally[A, A](trace) {
    @volatile private[process] var ready: A = z
    
    @volatile private[process] var count: Int = 0
    
    @volatile override var limit: Int = 0
    
    override def put(result: Try[A]) {
      if (result == null) throw new NullPointerException
      if (result.canTrap) {
        set(result.asInstanceOf[Nothing Else Throwable])
        ready = null.asInstanceOf[A]
      }
      else {
        var r = null.asInstanceOf[A]
        do r = ready
        while (r != null && !Unsafe.compareAndSwapObject(this, ReduceReadyOffset, r, null) ||
               r == null && !Unsafe.compareAndSwapObject(this, ReduceReadyOffset, null, result.bind))
        var n = 0
        if (r != null) {
          do n = count
          while (!Unsafe.compareAndSwapInt(this, ReduceCountOffset, n, n - 1))
          trace exec new Step(r, result.bind)
        }
        else {
          do n = count
          while (!Unsafe.compareAndSwapInt(this, ReduceCountOffset, n, n + 2))
          if (n == limit - 1) {
            set(result)
            ready = null.asInstanceOf[A]
          }
        }
      }
    }
    
    final override def putBind(value: A) {
      var r = null.asInstanceOf[A]
      do r = ready
      while (r != null && !Unsafe.compareAndSwapObject(this, ReduceReadyOffset, r, null) ||
             r == null && !Unsafe.compareAndSwapObject(this, ReduceReadyOffset, null, value))
      var n = 0
      if (r != null) {
        do n = count
        while (!Unsafe.compareAndSwapInt(this, ReduceCountOffset, n, n - 1))
        trace exec new Step(r, value)
      }
      else {
        do n = count
        while (!Unsafe.compareAndSwapInt(this, ReduceCountOffset, n, n + 2))
        if (n == limit - 1) {
          set(Bind(value))
          ready = null.asInstanceOf[A]
        }
      }
    }
    
    final override def putTrap(exception: Throwable) {
      set(Trap(exception))
      ready = null.asInstanceOf[A]
    }
    
    final override def commit() {
      val k = -limit
      limit = k
      val n = count // must read after updating limit
      if (n == k) {
        set(Maybe(ready)) // safely races with put
        ready = null.asInstanceOf[A]
      }
    }
    
    final class Step(a: A, b: A) extends AbstractFunction0[Unit] {
      override def apply(): Unit =
        try putBind(op(a, b))
        catch { case e: Throwable if Trap.isNonFatal(e) => putTrap(e) }
    }
  }
  
  private[basis] final class ForAll(trace: Trace) extends Tally[Boolean, Boolean](trace) {
    @volatile private[process] var count: Int = 0
    
    @volatile override var limit: Int = 0
    
    override def put(result: Try[Boolean]) {
      if (result == null) throw new NullPointerException
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, ForAllCountOffset, n, n + 1))
      k = limit // recheck limit
      if ((n < k || k < 0) && result.isFalse) set(False)
      else if (n == k - 1) set(result)
    }
    
    override def putBind(value: Boolean) {
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, ForAllCountOffset, n, n + 1))
      k = limit // recheck limit
      if ((n < k || k < 0) && !value) set(False)
      else if (n == k - 1) set(True)
    }
    
    override def putTrap(exception: Throwable): Unit = set(Trap(exception))
    
    override def commit() {
      val k = -limit
      limit = k
      val n = count // must read after updating limit
      if (n == k) set(True) // safely races with put
    }
  }
  
  private[basis] final class Exists(trace: Trace) extends Tally[Boolean, Boolean](trace) {
    @volatile private[process] var count: Int = 0
    
    @volatile override var limit: Int = 0
    
    final override def put(result: Try[Boolean]) {
      if (result == null) throw new NullPointerException
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, ExistsCountOffset, n, n + 1))
      k = limit // recheck limit
      if ((n < k || k < 0) && result.isTrue) set(True)
      else if (n == k - 1) set(result)
    }
    
    final override def putBind(value: Boolean) {
      var n = 0
      var k = 0
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, ExistsCountOffset, n, n + 1))
      k = limit // recheck limit
      if ((n < k || k < 0) && value) set(True)
      else if (n == k - 1) set(False)
    }
    
    final override def putTrap(exception: Throwable): Unit = set(Trap(exception))
    
    final override def commit() {
      val k = -limit
      limit = k
      val n = count // must read after updating limit
      if (n == k) set(False) // safely races with put
    }
  }
  
  private[basis] final class Count(trace: Trace) extends Tally[Int, Int](trace) {
    @volatile private[process] var total: Int = 0
    
    @volatile private[process] var count: Int = 0
    
    @volatile override var limit: Int = 0
    
    final override def put(result: Try[Int]) {
      if (result == null) throw new NullPointerException
      if (result.canBind) putBind(result.bind)
      else set(result.asInstanceOf[Nothing Else Throwable])
    }
    
    final override def putBind(value: Int) {
      var t = 0
      var n = 0
      var k = 0
      do { t = total; n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, CountTotalOffset, t, t + value))
      do { n = count; k = limit }
      while ((n < k || k < 0) && !Unsafe.compareAndSwapInt(this, CountCountOffset, n, n + 1))
      k = limit // recheck limit
      if (n == k - 1) set(Bind(total))
    }
    
    final override def putTrap(exception: Throwable): Unit = set(Trap(exception))
    
    final override def commit() {
      val k = -limit
      limit = k
      val n = count // must read after updating limit
      if (n == k) set(Bind(total)) // safely races with last put
    }
  }
  
  private[basis] final class PutAll[-A](thunk: () => A, tally: Tally[A, _]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      try tally putBind thunk() catch { case e: Throwable if Trap.isNonFatal(e) => tally putTrap e }
  }
  
  private[basis] final class PutAny[-A](thunk: () => A, tally: Tally[A, _]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!tally.isSet) (try tally putBind thunk() catch { case e: Throwable if Trap.isNonFatal(e) => tally putTrap e })
  }
}
