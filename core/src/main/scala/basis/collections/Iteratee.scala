//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._
import basis.text._
import basis.util._

trait Iteratee[-I, +O] {
  def isCont: Boolean = true

  def isDone: Boolean = false

  def isError: Boolean = false

  def asDone: Iteratee[Any, O] = asInstanceOf[Iteratee[Any, O]]

  def asError: Iteratee[Any, Nothing] = asInstanceOf[Iteratee[Any, Nothing]]

  def state: Maybe[O] = Trap

  def bind: O = state.bind

  def trap: Any = state.trap

  def feed(input: Iterator[I]): Iteratee[I, O]

  def run(input: Iterator[I]): Iteratee[I, O] = {
    var next = this
    while (!input.isEmpty && next.isCont) next = next.feed(input)
    if (input.isEmpty && !input.isDone && next.isCont) next = next.feed(Iterator.done)
    next
  }

  def map[P](f: O => P): Iteratee[I, P] =
    new Iteratee.Map[I, O, P](this, f)

  def flatMap[J <: I, P](f: O => Iteratee[J, P]): Iteratee[J, P] =
    new Iteratee.FlatMap[J, O, P](this, f)

  def recover[P >: O](pf: PartialFunction[Any, P]): Iteratee[I, P] =
    new Iteratee.Recover[I, P](this, pf)

  def recoverWith[J <: I, P >: O](pf: PartialFunction[Any, Iteratee[J, P]]): Iteratee[J, P] =
    new Iteratee.RecoverWith[J, P](this, pf)

  def <~ [J <: I](that: Iteratee[J, Any]): Iteratee[J, O] =
    new Iteratee.JoinLeft[J, O](this, that)

  def ~> [J <: I, P](that: Iteratee[J, P]): Iteratee[J, P] =
    new Iteratee.JoinRight[J, P](this, that)

  def <~> [J <: I, P](that: Iteratee[J, P]): Iteratee[J, (O, P)] =
    new Iteratee.Join[J, O, P](this, that)

  def *> [P](that: Iteratee[O, P]): Iteratee[I, P] =
    new Iteratee.ComposeRight[I, O, P](this, that)
}

object Iteratee {
  def done[@specialized(Int, Long, Float, Double, Boolean) O](bind: O): Iteratee[Any, O] = new Done[O](bind)

  def error(trap: Any): Iteratee[Any, Nothing] = new Error(trap)

  private final class Done[+O](override val bind: O) extends Iteratee[Any, O] {
    override def isCont: Boolean = false

    override def isDone: Boolean = true

    override def state: Maybe[O] = Bind(bind)

    override def feed(input: Iterator[Any]): Iteratee[Any, O] = this

    override def recover[P >: O](pf: PartialFunction[Any, P]): Iteratee[Any, P] = this

    override def recoverWith[J <: Any, P >: O](pf: PartialFunction[Any, Iteratee[J, P]]): Iteratee[J, P] = this

    override def equals(other: Any): Boolean = other match {
      case that: Done[_] => bind.equals(that.bind)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Done[O]], bind.hashCode))
    }

    override def toString: String =
      (String.Builder~"Iteratee"~'.'~"done"~'('~>bind~')').state
  }

  private final class Error(override val trap: Any) extends Iteratee[Any, Nothing] {
    override def isCont: Boolean = false

    override def isError: Boolean = true

    override def state: Maybe[Nothing] = Trap(trap)

    override def feed(input: Iterator[Any]): Iteratee[Any, Nothing] = this

    override def map[P](f: Nothing => P): Iteratee[Any, P] = this

    override def flatMap[J <: Any, P](f: Nothing => Iteratee[J, P]): Iteratee[J, P] = this

    override def equals(other: Any): Boolean = other match {
      case that: Error => trap.equals(that.trap)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Error], trap.hashCode))
    }

    override def toString: String =
      (String.Builder~"Iteratee"~'.'~"error"~'('~>trap~')').state
  }

  private final class Map[-I, O, +P](
      protected val self: Iteratee[I, O],
      protected val f: O => P)
    extends Iteratee[I, P] {

    override def feed(input: Iterator[I]): Iteratee[I, P] = {
      val next = self.feed(input)
      if (next.isDone) done(f(next.bind))
      else next.map(f)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Map[_, _, _] => self.equals(that.self) && f.equals(that.f)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Map[I, O, P]], self.hashCode), f.hashCode))
    }

    override def toString: String =
      (String.Builder~>self~'.'~"map"~'('~>f~')').state
  }

  private final class FlatMap[-I, O, +P](
      protected val self: Iteratee[I, O],
      protected val f: O => Iteratee[I, P])
    extends Iteratee[I, P] {

    override def feed(input: Iterator[I]): Iteratee[I, P] = {
      val next = self.feed(input)
      if (next.isDone) f(next.bind)
      else next.flatMap(f)
    }

    override def equals(other: Any): Boolean = other match {
      case that: FlatMap[_, _, _] => self.equals(that.self) && f.equals(that.f)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[FlatMap[I, O, P]], self.hashCode), f.hashCode))
    }

    override def toString: String =
      (String.Builder~>self~'.'~"flatMap"~'('~>f~')').state
  }

  private final class Recover[-I, +O](
      protected val self: Iteratee[I, O],
      protected val pf: PartialFunction[Any, O])
    extends Iteratee[I, O] {

    override def feed(input: Iterator[I]): Iteratee[I, O] = {
      val next = self.feed(input)
      if (next.isDone) next
      else if (next.isError && pf.isDefinedAt(next.trap)) done(pf(next.trap))
      else next.recover(pf)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Recover[_, _] => self.equals(that.self) && pf.equals(that.pf)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Recover[I, O]], self.hashCode), pf.hashCode))
    }

    override def toString: String =
      (String.Builder~>self~'.'~"recover"~'('~>pf~')').state
  }

  private final class RecoverWith[-I, +O](
      protected val self: Iteratee[I, O],
      protected val pf: PartialFunction[Any, Iteratee[I, O]])
    extends Iteratee[I, O] {

    override def feed(input: Iterator[I]): Iteratee[I, O] = {
      val next = self.feed(input)
      if (next.isDone) next
      else if (next.isError && pf.isDefinedAt(next.trap)) pf(next.trap)
      else next.recoverWith(pf)
    }

    override def equals(other: Any): Boolean = other match {
      case that: RecoverWith[_, _] => self.equals(that.self) && pf.equals(that.pf)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[RecoverWith[I, O]], self.hashCode), pf.hashCode))
    }

    override def toString: String =
      (String.Builder~>self~'.'~"recoverWith"~'('~>pf~')').state
  }

  private final class JoinLeft[-I, +O](
      protected val lhs: Iteratee[I, O],
      protected val rhs: Iteratee[I, Any])
    extends Iteratee[I, O] {

    override def feed(input: Iterator[I]): Iteratee[I, O] = {
      val next = lhs.feed(input)
      if (next.isDone) new JoinLeftRight(next.bind, rhs)
      else if (!next.isError) new JoinLeft[I, O](next, rhs)
      else next.asError
    }

    override def equals(other: Any): Boolean = other match {
      case that: JoinLeft[_, _] => lhs.equals(that.lhs) && rhs.equals(that.rhs)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[JoinLeft[I, O]], lhs.hashCode), rhs.hashCode))
    }

    override def toString: String =
      (String.Builder~'('~>lhs~" <~ "~>rhs~')').state
  }

  private final class JoinLeftRight[-I, +O](
      protected val lhs: O,
      protected val rhs: Iteratee[I, Any])
    extends Iteratee[I, O] {

    override def feed(input: Iterator[I]): Iteratee[I, O] = {
      val next = rhs.feed(input)
      if (next.isDone) done(lhs)
      else if (!next.isError) new JoinLeftRight(lhs, next)
      else next.asError
    }

    override def equals(other: Any): Boolean = other match {
      case that: JoinLeftRight[_, _] => lhs.equals(that.lhs) && rhs.equals(that.rhs)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[JoinLeftRight[I, O]], lhs.hashCode), rhs.hashCode))
    }

    override def toString: String =
      (String.Builder~'('~>lhs~" <~ "~>rhs~')').state
  }

  private final class JoinRight[-I, +O](
      protected val lhs: Iteratee[I, Any],
      protected val rhs: Iteratee[I, O])
    extends Iteratee[I, O] {

    override def feed(input: Iterator[I]): Iteratee[I, O] = {
      val next = lhs.feed(input)
      if (next.isDone) rhs
      else if (!next.isError) new JoinRight[I, O](next, rhs)
      else next.asError
    }

    override def equals(other: Any): Boolean = other match {
      case that: JoinRight[_, _] => lhs.equals(that.lhs) && rhs.equals(that.rhs)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[JoinRight[I, O]], lhs.hashCode), rhs.hashCode))
    }

    override def toString: String =
      (String.Builder~'('~>lhs~" ~> "~>rhs~')').state
  }

  private final class Join[-I, +O, +P](
      protected val lhs: Iteratee[I, O],
      protected val rhs: Iteratee[I, P])
    extends Iteratee[I, (O, P)] {

    override def feed(input: Iterator[I]): Iteratee[I, (O, P)] = {
      val next = lhs.feed(input)
      if (next.isDone) new JoinRest[I, O, P](next.bind, rhs)
      else if (!next.isError) new Join[I, O, P](next, rhs)
      else next.asError
    }

    override def equals(other: Any): Boolean = other match {
      case that: Join[_, _, _] => lhs.equals(that.lhs) && rhs.equals(that.rhs)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Join[I, O, P]], lhs.hashCode), rhs.hashCode))
    }

    override def toString: String =
      (String.Builder~'('~>lhs~" <~> "~>rhs~')').state
  }

  private final class JoinRest[-I, +O, +P](
      protected val lhs: O,
      protected val rhs: Iteratee[I, P])
    extends Iteratee[I, (O, P)] {

    override def feed(input: Iterator[I]): Iteratee[I, (O, P)] = {
      val next = rhs.feed(input)
      if (next.isDone) done(lhs -> next.bind)
      else if (!next.isError) new JoinRest[I, O, P](lhs, next)
      else next.asError
    }

    override def equals(other: Any): Boolean = other match {
      case that: JoinRest[_, _, _] => lhs.equals(that.lhs) && rhs.equals(that.rhs)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[JoinRest[I, O, P]], lhs.hashCode), rhs.hashCode))
    }

    override def toString: String =
      (String.Builder~'('~>lhs~" <~> "~>rhs~')').state
  }

  private final class ComposeRight[-I, O, +P](
      protected val lhs: Iteratee[I, O],
      protected val rhs: Iteratee[O, P])
    extends Iteratee[I, P] {

    override def feed(input: Iterator[I]): Iteratee[I, P] = {
      val enumerator = new Enumerator[I, O](lhs, input)
      val next = rhs.feed(enumerator)
      if (next.isDone) Iteratee.done(next.bind)
      else if (!next.isError) new ComposeRight[I, O, P](enumerator.next, next)
      else next.asError
    }

    override def equals(other: Any): Boolean = other match {
      case that: ComposeRight[_, _, _] => lhs.equals(that.lhs) && rhs.equals(that.rhs)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[ComposeRight[I, O, P]], lhs.hashCode), rhs.hashCode))
    }

    override def toString: String =
      (String.Builder~'('~>lhs~" *> "~>rhs~')').state
  }

  private final class Enumerator[I, O](
      private[this] val self: Iteratee[I, O],
      var next: Iteratee[I, O],
      private[this] val input: Iterator[I])
    extends Iterator[O] {

    def this(self: Iteratee[I, O], input: Iterator[I]) = this(self, self, input)

    override def isDone: Boolean =
      !next.isDone && input.isDone

    override def isEmpty: Boolean =
      !next.isDone && {
        while (!input.isEmpty && next.isCont) next = next.feed(input)
        !next.isDone
      }

    override def head: O =
      if (next.isDone) next.bind
      else {
        while (!input.isEmpty && next.isCont) next = next.feed(input)
        if (next.isDone) next.bind else Iterator.empty.head
      }

    override def step(): Unit =
      if (next.isDone) next = self
      else {
        while (!input.isEmpty && next.isCont) next = next.feed(input)
        if (!next.isDone) Iterator.empty.step()
      }

    override def dup: Iterator[O] = new Enumerator[I, O](self, next, input.dup)
  }

  abstract class AndThen[-I, +O, +P](self: Iteratee[I, O]) extends Iteratee[I, P] {
    override def feed(input: Iterator[I]): Iteratee[I, P] = {
      val next = self.feed(input)
      if (next.isDone) done(next.bind)
      else if (!next.isError) cont(next)
      else next.asError
    }

    protected[this] def done(output: O): Iteratee[I, P]

    protected[this] def cont(next: Iteratee[I, O]): Iteratee[I, P]

    protected[this] def error(next: Iteratee[Any, Nothing]): Iteratee[I, P] = next
  }

  abstract class Step[@specialized(Int, Long, Float, Double, Boolean) -I, +O] extends Iteratee[I, O] {
    override def feed(input: Iterator[I]): Iteratee[I, O] = {
      if (!input.isEmpty) {
        val elem = input.head
        val next = step(elem)
        if (accept(elem, next)) input.step()
        next
      }
      else if (!input.isDone) this
      else eof
    }

    protected[this] def step(elem: I): Iteratee[I, O]

    protected[this] def accept(elem: I, next: Iteratee[I, O]): Boolean = !next.isError

    protected[this] def done(bind: O): Iteratee[I, O] = Iteratee.done(bind)

    protected[this] def error(found: String): Iteratee[I, O] = {
      val expected = this.expected
      Iteratee.error {
        if (expected.length == 0 && found.length == 0) "unexpected input"
        else if (expected.length == 0) (String.Builder~"unexpected "~found).state
        else if (found.length == 0) (String.Builder~"expected "~expected).state
        else (String.Builder~"expected "~expected~", but found "~found).state
      }
    }

    protected[this] def eof: Iteratee[I, O] = {
      val expected = this.expected
      Iteratee.error {
        if (expected.length == 0) "unexpected end of input"
        else (String.Builder~"expected "~expected~", but found end of input").state
      }
    }

    protected def expected: String = ""
  }
}
