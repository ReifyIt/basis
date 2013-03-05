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
    val to = new Tally.Reduce(z)(op)(trace)
    to.limit = 1
    these.segments traverse new GeneralEnumeratorOps.RelayReduce(op)(to)(trace)
    to.commit()
    to
  }
  
  def reduce[B >: A](op: (B, B) => B)(implicit trace: Trace): Relay[B] = {
    val to = new Tally.Reduce(null.asInstanceOf[B])(op)(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayReduce(op)(to)(trace)
    to.commit()
    to
  }
  
  def find(p: A => Boolean)(implicit trace: Trace): Relay[A] = {
    val to = new Tally.JoinFirst(p)(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayFind(p)(to)(trace)
    to.commit()
    to
  }
  
  def forall(p: A => Boolean)(implicit trace: Trace): Relay[Boolean] = {
    val to = new Tally.ForAll(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayForAll(p)(to)(trace)
    to.commit()
    to
  }
  
  def exists(p: A => Boolean)(implicit trace: Trace): Relay[Boolean] = {
    val to = new Tally.Exists(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayExists(p)(to)(trace)
    to.commit()
    to
  }
  
  def count(p: A => Boolean)(implicit trace: Trace): Relay[Int] = {
    val to = new Tally.Count(trace)
    these.segments traverse new GeneralEnumeratorOps.RelayCount(p)(to)(trace)
    to.commit()
    to
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
  
  final class ReduceThunk[-A](xs: Enumerator[A])(op: (A, A) => A)(to: Tally.Reduce[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      try to putBind (xs reduce op) catch { case e: Throwable if Trap.isNonFatal(e) => to putTrap e }
  }
  
  final class RelayReduce[-A](op: (A, A) => A)(to: Tally.Reduce[A])(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      to.limit -= 1
      trace exec new ReduceThunk(xs)(op)(to)
    }
  }
  
  final class FindThunk[-A](xs: Enumerator[A])(p: A => Boolean)(to: Tally.JoinFirst[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!to.isSet) to put (try xs find p orElse Trap catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  final class RelayFind[-A](p: A => Boolean)(to: Tally.JoinFirst[A])(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      if (!to.isSet) {
        to.limit -= 1
        trace exec new FindThunk(xs)(p)(to)
      }
    }
  }
  
  final class ForAllThunk[-A](xs: Enumerator[A])(p: A => Boolean)(to: Tally.ForAll) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!to.isSet) try to putBind (xs forall p) catch { case e: Throwable if Trap.isNonFatal(e) => to putTrap e }
  }
  
  final class RelayForAll[-A](p: A => Boolean)(to: Tally.ForAll)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      if (!to.isSet) {
        to.limit -= 1
        trace exec new ForAllThunk(xs)(p)(to)
      }
    }
  }
  
  final class ExistsThunk[-A](xs: Enumerator[A])(p: A => Boolean)(to: Tally.Exists) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      if (!to.isSet) try to putBind (xs exists p) catch { case e: Throwable if Trap.isNonFatal(e) => to putTrap e }
  }
  
  final class RelayExists[-A](p: A => Boolean)(to: Tally.Exists)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      if (!to.isSet) {
        to.limit -= 1
        trace exec new ExistsThunk(xs)(p)(to)
      }
    }
  }
  
  final class CountThunk[-A](xs: Enumerator[A])(p: A => Boolean)(to: Tally.Count) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      try to putBind (xs count p) catch { case e: Throwable if Trap.isNonFatal(e) => to putTrap e }
  }
  
  final class RelayCount[-A](p: A => Boolean)(to: Tally.Count)(trace: Trace) extends AbstractFunction1[Enumerator[A], Unit] {
    override def apply(xs: Enumerator[A]) {
      to.limit -= 1
      trace exec new CountThunk(xs)(p)(to)
    }
  }
}
