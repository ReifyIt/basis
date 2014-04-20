//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A statefully traversable collection of elements.
  *
  * @define collection  iterator
  */
trait Iterator[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Iterator[_]] with Traverser[A] {

  def isDone: Boolean = false

  def isEmpty: Boolean

  def head: A

  def step(): Unit

  def dup: Iterator[A]

  override def traverse(f: A => Unit): Unit =
    while (!isEmpty) { f(head); step() }
}

object Iterator {
  def empty[A]: Iterator[A] = Empty

  def done[A]: Iterator[A] = Done

  private object Empty extends Iterator[Nothing] {
    override def isDone: Boolean = false

    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException("head of empty iterator")

    override def step(): Unit = throw new UnsupportedOperationException("empty iterator step")

    override def dup: Iterator[Nothing] = this

    override def traverse(f: Nothing => Unit): Unit = ()

    override def toString: String = "Iterator.empty"
  }

  private object Done extends Iterator[Nothing] {
    override def isDone: Boolean = true

    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException("head of empty iterator")

    override def step(): Unit = throw new UnsupportedOperationException("empty iterator step")

    override def dup: Iterator[Nothing] = this

    override def traverse(f: Nothing => Unit): Unit = ()

    override def toString: String = "Iterator.done"
  }
}
