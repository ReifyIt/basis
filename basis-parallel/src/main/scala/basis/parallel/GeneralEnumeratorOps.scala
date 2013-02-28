/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.parallel

import basis.collections._
import basis.control._
import basis.dispatch._
import basis.dispatch.process._

final class GeneralEnumeratorOps[+A](val these: Compound[Enumerator[A]]) extends AnyVal {
  def foreach[U](f: A => U)(implicit trace: Trace): Unit =
    these.segments traverse new GeneralEnumeratorOps.RelayForeach(f)(trace)
  
  def fold[B >: A](z: B)(op: (B, B) => B)(implicit trace: Trace): Relay[B] = {
    val t = new Thunk.Reduce(z)(op)(trace)
    t.limit = 1
    these.segments traverse new GeneralEnumeratorOps.RelayReduce(op)(t)(trace)
    t.commit()
    t
  }
  
  def reduce[B >: A](op: (B, B) => B)(implicit trace: Trace): Relay[B] = {
    val t = new Thunk.Reduce(null.asInstanceOf[B])(op)(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayReduce(op)(t)(trace)
    t.commit()
    t
  }
  
  def find(p: A => Boolean)(implicit trace: Trace): Relay[A] = {
    val t = new Thunk.JoinFirst(p)(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayFind(p)(t)(trace)
    t.commit()
    t
  }
  
  def forall(p: A => Boolean)(implicit trace: Trace): Relay[Boolean] = {
    val t = new Thunk.ForAll(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayForAll(p)(t)(trace)
    t.commit()
    t
  }
  
  def exists(p: A => Boolean)(implicit trace: Trace): Relay[Boolean] = {
    val t = new Thunk.Exists(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayExists(p)(t)(trace)
    t.commit()
    t
  }
  
  def count(p: A => Boolean)(implicit trace: Trace): Relay[Int] = {
    val t = new Thunk.Count(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayCount(p)(t)(trace)
    t.commit()
    t
  }
}

private[parallel] object GeneralEnumeratorOps {
  import basis.sequential.general.{GeneralEnumeratorOps => SequentialGeneralEnumeratorOps}
  import scala.runtime.AbstractFunction0
  import scala.runtime.AbstractFunction1
  
  final class ForeachThunk[-A](xs: Enumerator[A])(f: A => _) extends AbstractFunction0[Unit] {
    override def apply(): Unit = xs foreach f
  }
  
  final class RelayForeach[-A](f: A => _)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]): Unit = trace exec new ForeachThunk(xs)(f)
  }
  
  final class ReduceThunk[-A](xs: Enumerator[A])(op: (A, A) => A)(t: Thunk.Reduce[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      try t putBind (xs reduce op) catch { case e: Throwable if Trap.isNonFatal(e) => t putTrap e }
  }
  
  final class RelayReduce[-A](op: (A, A) => A)(t: Thunk.Reduce[A])(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      t.limit -= 1
      trace exec new ReduceThunk(xs)(op)(t)
    }
  }
  
  final class FindThunk[-A](xs: Enumerator[A])(p: A => Boolean)(t: Thunk.JoinFirst[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!t.isSet) t put (try xs find p orElse Trap catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  final class RelayFind[-A](p: A => Boolean)(t: Thunk.JoinFirst[A])(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      if (!t.isSet) {
        t.limit -= 1
        trace exec new FindThunk(xs)(p)(t)
      }
    }
  }
  
  final class ForAllThunk[-A](xs: Enumerator[A])(p: A => Boolean)(t: Thunk.ForAll) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!t.isSet) try t putBind (xs forall p) catch { case e: Throwable if Trap.isNonFatal(e) => t putTrap e }
  }
  
  final class RelayForAll[-A](p: A => Boolean)(t: Thunk.ForAll)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      if (!t.isSet) {
        t.limit -= 1
        trace exec new ForAllThunk(xs)(p)(t)
      }
    }
  }
  
  final class ExistsThunk[-A](xs: Enumerator[A])(p: A => Boolean)(t: Thunk.Exists) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!t.isSet) try t putBind (xs exists p) catch { case e: Throwable if Trap.isNonFatal(e) => t putTrap e }
  }
  
  final class RelayExists[-A](p: A => Boolean)(t: Thunk.Exists)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      if (!t.isSet) {
        t.limit -= 1
        trace exec new ExistsThunk(xs)(p)(t)
      }
    }
  }
  
  final class CountThunk[-A](xs: Enumerator[A])(p: A => Boolean)(t: Thunk.Count) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      try t putBind (xs count p) catch { case e: Throwable if Trap.isNonFatal(e) => t putTrap e }
  }
  
  final class RelayCount[-A](p: A => Boolean)(t: Thunk.Count)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      t.limit -= 1
      trace exec new CountThunk(xs)(p)(t)
    }
  }
}
